CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:48Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121125948  20190405100756  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�����1   @�����b@0� ě���dl1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @&ff@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$�C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyffD��D�FfD�|�D��fD�fD�6fD��3D��fD�fD�@ D��fD�� D�3D�C3D�p D�� D���D�9�D�vfD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>z@��
@��
AQ�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Ba�GBiz�Bqz�Byz�B��qB��qB��qB��qB��qB��B��qB��qB��qB��qB��qB��B��>B��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�CEC^�C^�C^�C ^�C"^�C$xRC&^�C(^�C*^�C,EC.^�C0^�C2^�C4^�C6^�C8^�C:EC<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CPxRCR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy~D��D�R=D���D��=D�"=D�B=D��
D��=D�=D�K�D��=D���D�
D�O
D�{�D���D��D�EqD�=D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�G�A�M�A�M�A�XA�XA�XA�XA�XA�VA�VA�XA�^5A�dZA�ffA�hsA�hsA�jA�l�A�n�A�n�A�l�A�l�A�p�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�x�A�z�A�z�A�z�A�v�A�p�A�XA�;dA�ȴA�XA��Aק�A�?}A�K�A���A�r�Aӥ�A�%AѰ!AЛ�Aχ+A�A��A͗�A�K�A�$�A�;dA�(�Aʟ�A��A�~�A�"�AȾwA�hsA���AǓuA���A�C�AŋDAė�A���A��RA���A��7A���A��A�JA�ȴA��A��A�7LA��PA��A���A�n�A�9XA�l�A�ffA�JA��jA�A�A�O�A�A���A���A��jA��A��PA��\A��A���A��A�|�A�VA��FA�/A�t�A��DA���A�ƨA���A�x�A���A~�\A|�RA{?}Ay�hAs�#AnjAh-Ad�Ab�9A`�jA_O�A\ĜAU�AR��AQ��APbAM7LAL�AJA�AH1AF~�AD�ABbA>�A:VA933A8��A8��A7�#A6�A3?}A1p�A/;dA-�mA-S�A,ȴA+�;A)��A( �A'�hA&�A&M�A&bA%�A%`BA%"�A$��A$��A%7LA$��A#A#VA"9XA!�wA ��A bNA -A   A JA��A�
A�FA��A�AXA&�A��A9XA�;A��A  A&�A1A`BAl�AȴA�TAoA�\Av�AbNAz�AbAbNA�-AdZA�A�uA��A1'A?}Az�A  A\)A"�A�A�+A�AS�A
��A	�TA	A�9AA�A�A��A�-A|�A�yAbA��A�wAhsA(�AZA�jAE�A�Az�A5?AZAM�Ax�A �A 5?@��
@�o@�V@��h@�bN@�@�X@��j@�(�@�\)@�v�@���@���@���@�F@�@�n�@���@���@�7L@�j@�b@�ƨ@�dZ@�@@�5?@���@�h@�X@��`@��@��@�V@��@���@�z�@�1@睲@�dZ@�S�@�R@�h@���@�r�@�A�@� �@�1@��m@�ƨ@㕁@�S�@��@�Z@�-@ܬ@�K�@��@�ȴ@�ff@ٙ�@�X@�%@؃@�9X@��
@�@֗�@�@�?}@��/@�(�@ӝ�@�;d@��@ҏ\@�ff@��@��T@�@Ѳ-@ёh@�hs@�7L@��`@Ѓ@��@ύP@�o@��y@���@ΰ!@�n�@�J@Ͳ-@�&�@̬@�9X@��
@ˍP@�l�@�+@��@�V@��@�`B@�%@ȋD@�j@�1@���@Ǯ@ǍP@�C�@ƸR@�ff@�=q@�{@��@��T@��#@Ų-@�/@���@�z�@��
@Å@�S�@�"�@\@�=q@�$�@�J@���@��@�p�@��/@�Z@�b@��F@��P@�+@��H@��+@�-@��^@�O�@�V@��@��j@�r�@�(�@�  @��F@�t�@�;d@���@���@�E�@��@���@��7@���@� �@�ƨ@�+@�o@��@��H@�ȴ@���@�n�@�E�@�$�@��@���@��@���@��`@��/@�Ĝ@���@�I�@��m@�l�@�"�@���@��\@��@���@�V@���@�A�@�1@�ƨ@���@�dZ@���@��\@�{@��^@�G�@�V@�j@�9X@��@��@�33@�^5@��^@�hs@��@���@��9@���@�r�@��m@�S�@���@���@�M�@��^@�%@���@��@�bN@�  @��F@��@�l�@�
=@���@��\@�M�@��@�/@���@�j@��;@�ƨ@���@�@�V@���@�X@�V@�9X@�l�@� �@�+@�l�@�`B@x �@nff@dz�@^{@V{@L1@CdZ@9�^@4�j@,��@%�T@ �`@�
@ �@�H@;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�M�A�G�A�M�A�M�A�XA�XA�XA�XA�XA�VA�VA�XA�^5A�dZA�ffA�hsA�hsA�jA�l�A�n�A�n�A�l�A�l�A�p�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�x�A�z�A�z�A�z�A�v�A�p�A�XA�;dA�ȴA�XA��Aק�A�?}A�K�A���A�r�Aӥ�A�%AѰ!AЛ�Aχ+A�A��A͗�A�K�A�$�A�;dA�(�Aʟ�A��A�~�A�"�AȾwA�hsA���AǓuA���A�C�AŋDAė�A���A��RA���A��7A���A��A�JA�ȴA��A��A�7LA��PA��A���A�n�A�9XA�l�A�ffA�JA��jA�A�A�O�A�A���A���A��jA��A��PA��\A��A���A��A�|�A�VA��FA�/A�t�A��DA���A�ƨA���A�x�A���A~�\A|�RA{?}Ay�hAs�#AnjAh-Ad�Ab�9A`�jA_O�A\ĜAU�AR��AQ��APbAM7LAL�AJA�AH1AF~�AD�ABbA>�A:VA933A8��A8��A7�#A6�A3?}A1p�A/;dA-�mA-S�A,ȴA+�;A)��A( �A'�hA&�A&M�A&bA%�A%`BA%"�A$��A$��A%7LA$��A#A#VA"9XA!�wA ��A bNA -A   A JA��A�
A�FA��A�AXA&�A��A9XA�;A��A  A&�A1A`BAl�AȴA�TAoA�\Av�AbNAz�AbAbNA�-AdZA�A�uA��A1'A?}Az�A  A\)A"�A�A�+A�AS�A
��A	�TA	A�9AA�A�A��A�-A|�A�yAbA��A�wAhsA(�AZA�jAE�A�Az�A5?AZAM�Ax�A �A 5?@��
@�o@�V@��h@�bN@�@�X@��j@�(�@�\)@�v�@���@���@���@�F@�@�n�@���@���@�7L@�j@�b@�ƨ@�dZ@�@@�5?@���@�h@�X@��`@��@��@�V@��@���@�z�@�1@睲@�dZ@�S�@�R@�h@���@�r�@�A�@� �@�1@��m@�ƨ@㕁@�S�@��@�Z@�-@ܬ@�K�@��@�ȴ@�ff@ٙ�@�X@�%@؃@�9X@��
@�@֗�@�@�?}@��/@�(�@ӝ�@�;d@��@ҏ\@�ff@��@��T@�@Ѳ-@ёh@�hs@�7L@��`@Ѓ@��@ύP@�o@��y@���@ΰ!@�n�@�J@Ͳ-@�&�@̬@�9X@��
@ˍP@�l�@�+@��@�V@��@�`B@�%@ȋD@�j@�1@���@Ǯ@ǍP@�C�@ƸR@�ff@�=q@�{@��@��T@��#@Ų-@�/@���@�z�@��
@Å@�S�@�"�@\@�=q@�$�@�J@���@��@�p�@��/@�Z@�b@��F@��P@�+@��H@��+@�-@��^@�O�@�V@��@��j@�r�@�(�@�  @��F@�t�@�;d@���@���@�E�@��@���@��7@���@� �@�ƨ@�+@�o@��@��H@�ȴ@���@�n�@�E�@�$�@��@���@��@���@��`@��/@�Ĝ@���@�I�@��m@�l�@�"�@���@��\@��@���@�V@���@�A�@�1@�ƨ@���@�dZ@���@��\@�{@��^@�G�@�V@�j@�9X@��@��@�33@�^5@��^@�hs@��@���@��9@���@�r�@��m@�S�@���@���@�M�@��^@�%@���@��@�bN@�  @��F@��@�l�@�
=@���@��\@�M�@��@�/@���@�j@��;@�ƨ@���@�@�V@���@�X@�VG�O�@�l�@� �@�+@�l�@�`B@x �@nff@dz�@^{@V{@L1@CdZ@9�^@4�j@,��@%�T@ �`@�
@ �@�H@;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�}B
�wB
�wB
�wB
�wB
�wB
�qB
�dB
�RB
�B
�1B
{�B
x�B
{�B
�B
�DB
�\B
��B
��B
�!B
��B
��B
��B
B
��B
�B
�)BB�B�B0!BQ�BcTBr�By�B�=B�oB��B�B�9B�wB�;B��B1B\B�B)�B,B,B49BN�B49BD�BaHBhsBm�Br�B�B�Bp�BjBE�B%B�BB�-B��B�FB�!B�9B��B�1B��B�B��B��B�=B^5B�B
�;B
�wB
��B
o�B
B�B
�B
B	��B	�B	�B	�?B	��B	v�B	aHB	YB	N�B	D�B	5?B	�B	JB	%B��B��B�B�B�ZB�;B�B��B��B��B��B��B��B��BɺB��B��B��B��B��B��B��B��B��B�B�HB�B�B��B	  B	�B	+B	E�B	dZB	u�B	{�B	~�B	�B	�B	�=B	�oB	��B	��B	��B	B	��B	��B	��B	�#B	�HB	�ZB	�`B	�sB	�fB	�NB	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�NB	��B	��B	�dB	��B	��B	��B	��B	ȴB	��B	�jB	�LB	�RB	�jB	�XB	�LB	�3B	�?B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	��B	��B	��B	��B	�'B	�3B	�jB	B	ŢB	��B	��B	��B	�)B	�#B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB

=B
DB
DB
JB
DB
DB
�B
JB
{B
�B
�B
%�B
,B
1'B
?}B
A�B
B�B
H�B
N�B
W
B
[#B
bNB
ffB
k�B
o�B
r�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
�ZB
�XB
�XB
�ZB
�ZB
�ZB
�ZB
�ZB
�]B
�ZB
�ZB
�]B
�\B
�ZB
�ZB
�ZB
�ZB
�ZB
�XB
�ZB
�ZB
�ZB
�ZB
�XB
�]B
�aB
�bB
�ZB
�ZB
�]B
�bB
�cB
�]B
�]B
�aB
�YB
�YB
�YB
�WB
�ZB
�SB
�HB
�8B
��B
�B
{�B
x�B
{�B
��B
�(B
�=B
�oB
��B
�B
��B
��B
��B
�sB
��B
��B
�B �BtB�B0BQ�Bc7Br�By�B�B�PB��B��B�B�XB�B��BB<B�B)�B+�B+�B4BN�B4BD}Ba&BhRBmrBr�B��B��Bp�Bj`BE�BB�"B�B��B�%B�B�B��B�B��B��B�eB��B�B^BgB
�B
�PB
��B
owB
BkB
kB
�B	��B	�[B	��B	�B	�mB	v�B	a"B	X�B	N�B	DtB	5B	gB	"B	�B��B��B�yB�XB�0B�B��B��BкBϵBίBήB̦B˞BɎB˝BʖB˛BʗBʗBʖB˛BϵB��B��B�B�YB�zB��B��B	mB	*�B	EyB	d1B	u�B	{�B	~�B	��B	��B	�B	�DB	�\B	�pB	��B	�fB	̡B	ѿB	��B	��B	�B	�-B	�6B	�GB	�8B	�#B	��B	��B	иB	άB	��B	��B	άB	ʔB	˚B	̞B	̡B	��B	�#B	��B	ϴB	�8B	�WB	�XB	��B	иB	ȆB	�^B	�=B	�B	�$B	�<B	�.B	�B	�B	�B	��B	��B	��B	��B	��B	�sB	�UB	�YB	�YB	�JB	�FB	�_B	�xB	��B	��B	��B	�B	�>B	�bB	�tB	ͩB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѾB	ѽB	ѾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ϳB	˘B	̡B	˙B	˘B	˙B	˘B	̡B	̞B	̟B	άB	ΫB	ΪB	ΫB	ϱB	ϯB	еB	еB	ѻB	ѽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�&B	�+B	�-B	�+B	�,B	�*B	�+B	�,B	�1B	�1B	�3B	�7B	�5B	�<B	�<B	�>B	�>B	�@B	�?B	�EB	�EB	�?B	�DB	�HB	�NB	�PB	�OB	�PB	�SB	�XB	�TB	�]B	�cB	�iB	�jB	�iB	�bB	�hB	�gB	�pB	�vB	�uB	�sB	�zB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
B
B
B
	B
		B
	B
		B
		B
	B

B

B

B

B


B

B

B
B
B

B
B
B
B
B
G�O�B
B
IB
iB
�B
%�B
+�B
0�B
?LB
A\B
B^B
H�B
N�B
V�B
Z�B
bB
f6B
kXB
olB
r�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007562019040510075620190405100756  AO  ARCAADJP                                                                    20181121125948    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125948  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125948  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100756  IP                  G�O�G�O�G�O�                