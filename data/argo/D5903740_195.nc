CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-15T00:02:29Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170815000229  20190604095307  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�lm�4 1   @�mr>�@:�I�^�cOI�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B   B(  B0  B8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy#�D� �D�<{D���D�� D�
D�C3D�~fD�߮D�fD�F�D��Dǽ�D�D�.fDښ�D���D���D�A�D�y�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
A�A%�AE�Ae�A���A���A���A���A���A���A���A���B�GB	�GBz�Bz�B!z�B)z�B1z�B9�GBA{BI{BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��DdDd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�HDy;�D�{D�HRD��fD���D��D�O
D��=D��D�=D�R�D���D�ɚD��D�:=DڦfD��D��D�MqD�qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�hsA�p�A�x�A�x�A�v�A�t�A�p�A�n�A�x�A�r�A�dZA�jA�t�A�~�A�5?A��Aذ!A�A�l�A�x�A�$�A���A�Q�A���A��yAuA��A���A�`BA�{A�bNA�JA��A�A��A�"�A��A���A�bA�1'A��A�M�A��uA��A�7LA�E�A��jA��hA���A���A��+A��A��wA��A���A��`A��A�G�A� �A���A�p�A���A�bNA��A�ffA��A���A��A�A���A�%A�\)A�|�A��A�JA�7LA��yA�S�A�ffA��hA�r�A���A�oA�z�A�%A��A�v�A��A��A�VA���A���A�XA���A��A��A�bA~�uA}��A}%Az��Ay��Ax��Av��At��As�
As%Aq��Ap �AooAm��Am�Al�AkO�Aj�Ai33AhQ�AgdZAf��Af=qAd��AdJAb�HAa�mAaA_�A^=qA]/A\n�A[G�AZ��AZ(�AY+AXbNAW��AV��AVVAUp�ATZASAR��AQ��AQ�AQ33AO�wAO
=AMALffAK;dAJ5?AI�AG�AE"�AC�TAC�AChsAC7LABv�ABv�ABn�ABA@�HA@I�A?�A=��A=oA<E�A;��A:ȴA9�
A8�RA7�^A7?}A6��A4jA2�A1�TA0�`A0E�A0 �A/�-A/�A.�+A-�TA-G�A,�HA,bNA+��A+�FA*�/A*=qA)�A)VA(ȴA'�A'
=A&M�A%�-A$��A$Q�A$  A#�^A"M�A!�A E�A��A"�A��A�A��A��A�A �Al�A�+A{A��A\)A��A~�A��A-AƨA��A��A��AE�A�hA~�A�PA+A��A5?A�A
��A
$�A	��A�AJA�#A�A�A�TA�`AA�A�hAVA��A�AXA33A%A �@���@�=q@���@�33@��@�"�@��@�@�E�@�^@���@�K�@�{@�/@��
@��@�@��
@旍@�v�@��@���@�1@�K�@��u@�o@ݲ-@�@�=q@ڧ�@�5?@��`@��;@֗�@�%@�Q�@�l�@�&�@�9X@�;d@Ο�@�{@�`B@� �@˶F@�l�@�ff@���@�A�@�l�@�v�@ļj@�ȴ@���@�1@�dZ@��H@�$�@��^@�%@�Q�@�|�@�@�$�@���@�K�@��\@�`B@��@��
@�S�@��@�G�@�j@���@���@��R@�v�@�V@�-@���@���@��@�I�@��@�
=@���@�n�@�@�p�@��u@��@��@�
=@���@���@�`B@�X@���@�z�@�C�@�@��@��u@�1'@�ƨ@�|�@��H@�J@��^@���@��h@�x�@�/@���@��D@�b@��F@��@��!@�v�@�E�@�J@��h@� �@���@�v�@�x�@���@�9X@���@���@�@��h@�p�@�V@�Q�@��w@�S�@��@���@���@�ff@���@��-@�/@���@�I�@�(�@�b@���@�C�@��@��H@���@��!@���@�v�@��@��-@��7@�G�@��@�%@��@�1'@��P@�\)@�dZ@�C�@��@���@�n�@�n�@��@�`B@�/@���@�r�@��@��m@�dZ@��@��H@�ȴ@���@��@��@��R@�V@�@���@��@�hs@���@��T@���@���@�x�@�`B@�?}@�7L@���@��`@��j@���@�(�@��@�(�@�  @~�y@}�T@}`B@|�j@|�j@|�j@|�@|�@{�F@{dZ@{"�@z�!@y��@x�@xb@w��@w|�@w�@v�@vE�@u��@up�@u/@t�@t�@o��@j@c�@\�.@T�)@Lѷ@E��@@�O@;6z@4M@/=@)�@%;@!Q�@h�@\�@$t@"h@~(@7@�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�jA�hsA�p�A�x�A�x�A�v�A�t�A�p�A�n�A�x�A�r�A�dZA�jA�t�A�~�A�5?A��Aذ!A�A�l�A�x�A�$�A���A�Q�A���A��yAuA��A���A�`BA�{A�bNA�JA��A�A��A�"�A��A���A�bA�1'A��A�M�A��uA��A�7LA�E�A��jA��hA���A���A��+A��A��wA��A���A��`A��A�G�A� �A���A�p�A���A�bNA��A�ffA��A���A��A�A���A�%A�\)A�|�A��A�JA�7LA��yA�S�A�ffA��hA�r�A���A�oA�z�A�%A��A�v�A��A��A�VA���A���A�XA���A��A��A�bA~�uA}��A}%Az��Ay��Ax��Av��At��As�
As%Aq��Ap �AooAm��Am�Al�AkO�Aj�Ai33AhQ�AgdZAf��Af=qAd��AdJAb�HAa�mAaA_�A^=qA]/A\n�A[G�AZ��AZ(�AY+AXbNAW��AV��AVVAUp�ATZASAR��AQ��AQ�AQ33AO�wAO
=AMALffAK;dAJ5?AI�AG�AE"�AC�TAC�AChsAC7LABv�ABv�ABn�ABA@�HA@I�A?�A=��A=oA<E�A;��A:ȴA9�
A8�RA7�^A7?}A6��A4jA2�A1�TA0�`A0E�A0 �A/�-A/�A.�+A-�TA-G�A,�HA,bNA+��A+�FA*�/A*=qA)�A)VA(ȴA'�A'
=A&M�A%�-A$��A$Q�A$  A#�^A"M�A!�A E�A��A"�A��A�A��A��A�A �Al�A�+A{A��A\)A��A~�A��A-AƨA��A��A��AE�A�hA~�A�PA+A��A5?A�A
��A
$�A	��A�AJA�#A�A�A�TA�`AA�A�hAVA��A�AXA33A%A �@���@�=q@���@�33@��@�"�@��@�@�E�@�^@���@�K�@�{@�/@��
@��@�@��
@旍@�v�@��@���@�1@�K�@��u@�o@ݲ-@�@�=q@ڧ�@�5?@��`@��;@֗�@�%@�Q�@�l�@�&�@�9X@�;d@Ο�@�{@�`B@� �@˶F@�l�@�ff@���@�A�@�l�@�v�@ļj@�ȴ@���@�1@�dZ@��H@�$�@��^@�%@�Q�@�|�@�@�$�@���@�K�@��\@�`B@��@��
@�S�@��@�G�@�j@���@���@��R@�v�@�V@�-@���@���@��@�I�@��@�
=@���@�n�@�@�p�@��u@��@��@�
=@���@���@�`B@�X@���@�z�@�C�@�@��@��u@�1'@�ƨ@�|�@��H@�J@��^@���@��h@�x�@�/@���@��D@�b@��F@��@��!@�v�@�E�@�J@��h@� �@���@�v�@�x�@���@�9X@���@���@�@��h@�p�@�V@�Q�@��w@�S�@��@���@���@�ff@���@��-@�/@���@�I�@�(�@�b@���@�C�@��@��H@���@��!@���@�v�@��@��-@��7@�G�@��@�%@��@�1'@��P@�\)@�dZ@�C�@��@���@�n�@�n�@��@�`B@�/@���@�r�@��@��m@�dZ@��@��H@�ȴ@���@��@��@��R@�V@�@���@��@�hs@���@��T@���@���@�x�@�`B@�?}@�7L@���@��`@��j@���@�(�@��@�(�@�  @~�y@}�T@}`B@|�j@|�j@|�j@|�@|�@{�F@{dZ@{"�@z�!@y��@x�@xb@w��@w|�@w�@v�@vE�@u��@up�@u/@t�G�O�@o��@j@c�@\�.@T�)@Lѷ@E��@@�O@;6z@4M@/=@)�@%;@!Q�@h�@\�@$t@"h@~(@7@�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�yB�#B�hB��B�B�
B�dB��B�/B�B�
B��B��B��B�XB�XB�jB�dB��B��B�FB�B��B��B�hB�Bw�BhsB\)Be`Be`BN�B@�BK�BF�B@�B,BbB��B�fB�B��B��B�B%�B(�B(�B�BuB
=BB��B�B�
B��BĜB�XB�B��B��B�DB�oB�Bn�BK�B6FB+B�B{BPB�B�B�B�B�BVBDB  B
�B
�5B
�wB
��B
�%B
�B
�B
x�B
n�B
dZB
XB
J�B
D�B
=qB
2-B
#�B
�B
oB
DB
B	��B	�B	�B	�fB	�/B	��B	��B	ȴB	��B	�RB	�'B	��B	��B	��B	�hB	�DB	�B	|�B	w�B	p�B	k�B	ffB	bNB	^5B	XB	O�B	K�B	E�B	;dB	;dB	F�B	>wB	:^B	0!B	#�B	�B	�B	bB	%B��B��B��B��B	  B	B	%B		7B		7B	B	  B��B�B�B�B�mB�HB�#B��B��B��BÖB�9B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�JB�7B�+B�B�B�B� B}�By�Bw�Bt�Br�Bp�Bm�BiyBdZBaHB^5BYBW
BVBT�BS�BT�BT�BS�BN�BJ�BG�BC�BA�B?}B=qB=qB9XB9XB8RB7LB8RB6FB5?B49B33B1'B0!B/B.B,B)�B(�B'�B%�B%�B#�B"�B!�B �B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B&�B&�B'�B0!B5?B5?B9XB>wBB�BC�BB�BB�BC�BD�BJ�BM�BP�BS�BVBW
BYBZB]/B_;BbNBdZBffBiyBl�Bl�Bl�Bm�Br�Bx�B}�B~�B�B�B�B�B�1B�7B�7B�7B�7B�=B�DB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�FB�XB�qB��BĜBƨBǮBȴB��B��B��B��B��B��B�B�B�B�)B�5B�BB�HB�NB�NB�TB�ZB�`B�fB�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	%B	1B		7B	
=B	DB	JB	bB	oB	oB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	&�B	)�B	,B	/B	2-B	7LB	9XB	;dB	>wB	>wB	?}B	@�B	B�B	E�B	G�B	H�B	K�B	M�B	P�B	T�B	VB	XB	YB	ZB	\)B	`BB	aHB	bNB	ffB	iyB	k�B	m�B	p�B	p�B	q�B	t�B	v�B	x�B	y�B	y�B	|�B	��B	�}B	�_B	�_B	��B
B
�B
%FB
,�B
8lB
@�B
G�B
O�B
V9B
\CB
c�B
g�B
l�B
qvB
x8B
{0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB�ZB�B�JB��B�B��B�GBκB�B��B��B��B̮B�iB�7B�7B�IB�DB�`B�_B�&B��B��B��B�FB��Bw�BhTB\	Be?Be?BN�B@`BK�BF�B@`B+�BAB��B�FB��B��B��B�uB%�B(�B(�B�BPB
B�B��B�\B��BˡB�xB�3B��B��B�]B�B�LB��BnqBK�B6 B*�B�BWB*B�B�BrB�B]B/BB
��B
�B
�B
�SB
�kB
��B
��B
��B
x�B
npB
d3B
W�B
J�B
DwB
=KB
2B
#�B
|B
HB
B
�B	��B	�B	�eB	�@B	�B	��B	пB	ȎB	�bB	�,B	�B	��B	��B	�mB	�AB	�B	��B	|�B	w�B	p|B	k]B	f?B	b&B	^B	W�B	O�B	K�B	E{B	;=B	;>B	F}B	>MB	:8B	/�B	#�B	wB	XB	8B	�B��B��B��B��B��B	�B	�B		B		B	�B��B��B�B�uB�TB�BB�B��B��BαBʕB�jB�B��B��B�PB�zB��B��B��B��B��B��B��B�}B�wB�iB�\B�OB�DB�6B�4B�B�B��B��B��B��B�B}�By�Bw�Bt�Br�BpwBmeBiKBd1BaB^
BX�BV�BU�BT�BS�BT�BT�BS�BN�BJ�BG�BCjBA^B?RB=EB=CB9,B9,B8&B7B8'B6B5B4B3B0�B/�B.�B-�B+�B)�B(�B'�B%�B%�B#�B"�B!�B �B�B�B~ByBlBfB[BSBNBSB\BXBWBYBdBjBpB�B~B�B�B~B~B�B�BuBeBtB`BRBnB �B!�B!�B �B�B�BBxBoBsBrBrBqBqBwByBsBoB~B}B~BuBdBQBSBtB{B}B{B|BxBqBvByBoBpBpBpB�B&�B&�B'�B/�B5B5B9(B>GBBaBCeBB]BB^BChBDlBJ�BM�BP�BS�BU�BV�BX�BY�B\�B_BbBd+Bf6BiHBlZBl\Bl]Bm_BrBx�B}�B~�B��B��B��B��B��B�B�B�B�B�B�B�*B�PB�`B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�>B�XB�kB�wB�}BȄB˕BΦBϯBеBѼB��B��B��B��B��B�B�B�B�B�B�%B�+B�0B�2B�LB�[B�nB�{B�yB�B��B��B��B��B��B	 �B	�B	�B	�B	�B	�B		B	
B	B	B	0B	?B	>B	EB	JB	TB	bB	vB	|B	�B	�B	!�B	&�B	)�B	+�B	.�B	1�B	7B	9'B	;3B	>FB	>GB	?MB	@PB	B_B	EqB	G}B	H�B	K�B	M�B	P�B	T�B	U�B	W�B	X�B	Y�B	[�B	`B	aB	bB	f5B	iGB	kUB	m^B	ptB	ptB	qxB	t�B	v�B	x�B	y�B	y�G�O�B	��B	�LB	�/B	�/B	�~B
�B
�B
%B
,�B
8<B
@SB
G�B
O|B
V	B
\B
c�B
g�B
l�B
qGB
xB
{ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953072019060409530720190604095307  AO  ARCAADJP                                                                    20170815000229    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170815000229  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170815000229  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095307  IP                  G�O�G�O�G�O�                