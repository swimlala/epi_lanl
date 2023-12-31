CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:02Z AOML 3.0 creation; 2016-08-07T21:51:09Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221402  20160807145109  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_004                   2C  D   APEX                            6529                            072314                          846 @��1�o�1   @�����@2l�C���c�`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D��D�Y�D��3D���D�fD�,�D���D���D��fD�C3D��fD���D���D�C3DچfD���D�fD�FfD�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
A�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��B��B��>B��qB��qB��qBĽqBȽqB̽qB��BԽqBؽqBܽqB�qB�qB�qB�qB�qB��B��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�HD�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$D$��D%�D%��D&�D&�HD'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,�HD-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�{Dy��D��D�eqD��
D��qD�=D�8�D��qD��qD�=D�O
D��=D��D��D�O
Dڒ=D�ؤD�=D�R=D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A݋DA�l�A�XA�E�A�bA���A���AܶFAܩ�Aܣ�Aܛ�AܓuA܉7A�~�A�v�A�l�A�dZA�^5A�K�A�9XA��A�  A���A�ZAڙ�Aٕ�A�S�A�C�A�-A��A�A��HA�ƨAؼjAذ!A؁A�"�A���A։7A�I�A�~�Aк^A�p�A���A�"�Aơ�A� �AÕ�A¥�A���A�O�A��+A�"�A�M�A�ȴA��A���A��A��RA�hsA�&�A�x�A�ȴA�oA���A�`BA��#A�K�A�|�A�dZA���A��#A�
=A���A�9XA��9A�ȴA���A���A���A�v�A�ƨA���A��\A�&�A�E�A�x�A���A��+A�z�A��A� �A�l�A~�+AzZAwt�At��AsG�ArAp�Am�FAi�wAc�^Aa33A`=qA_��A^�AY�mAV�\AS�AP(�AO��AN��AM��AM?}AK|�AG�hAGoAF�jAFAC��ABr�A@�yA>�A;A8�+A5|�A0jA/"�A-`BA,(�A+S�A*��A)�A(��A({A&�`A&$�A$��A#"�A" �A!+A v�Ap�A7LA�A�DAp�A�jA��A/A�\A��Ar�A��Ap�AXA?}AA�An�A�FAbA�A�Al�A��A�RA=qA�A	��A��AA�A�^A;dA�RAffAn�Ap�AĜA�TA�A�A�`A��A{A�^A��Ax�Ax�A�A��A+A�yAĜA��AĜAȴAĜAȴA��A�9A�A~�A^5A(�A;d@���@�&�@�bN@�1@�dZ@��H@���@�v�@��@��@���@��\@�v�@�n�@�ff@�J@�Q�@�  @�F@��@�!@�@�\@�~�@�-@�@��@�&�@�9X@���@���@�E�@��T@���@��^@�O�@��/@웦@�t�@��@���@�^@�x�@�@�o@�@�7@�hs@�O�@��@�bN@�S�@�^@�7L@���@���@�bN@�b@�33@�@�/@ܛ�@�z�@�b@�dZ@�"�@�@ڇ+@�hs@�r�@��@��H@ղ-@���@��@���@�ff@�-@�J@щ7@�V@�9X@�"�@�V@���@�7L@̴9@�9X@�  @���@˥�@�33@��@�J@�G�@�/@�1'@�\)@�;d@�V@�`B@��@���@��@���@ě�@�9X@�+@�E�@��T@�@�x�@�7L@���@���@�9X@���@���@�S�@�@�=q@���@��@��-@�x�@��j@�I�@�A�@�(�@� �@�1@��m@�;d@���@�5?@�@�G�@��/@�Ĝ@��@�Q�@� �@��m@�|�@�\)@�;d@��y@���@�E�@��@���@���@�`B@���@�z�@� �@�t�@�"�@��y@���@��@�p�@�/@���@���@���@�Z@�ƨ@��P@�dZ@�S�@�;d@���@��7@��@���@�Ĝ@�bN@��@��R@�=q@�{@���@���@�Z@�b@�C�@���@�{@�p�@�V@��9@�z�@�bN@�A�@���@���@��@�l�@��+@�hs@�%@�Q�@�ƨ@�;d@�ff@�J@��#@���@�@��-@���@���@���@��7@�hs@���@�Q�@��@���@��F@���@�|�@�l�@�\)@�"�@��H@��+@���@��#@��-@�p�@�O�@�?}@��@��`@��j@���@���@���@��D@�j@�9X@�b@���@��m@�ƨ@�l�@�;d@�
=@��+@��7@�O�@�7L@�7L@��@�%@�V@��@��@��@��@�&�@�V@��j@��u@�j@�1@�l�@�C�@�+@�o@�@��@���@���@�~�@�M�@���@�G�@��@���@��-@�n�@�n�@}��@p�9@j�@d�D@[�
@R�\@G�@<�@5p�@.ȴ@+33@%V@ �`@�-@�y@^5@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A݋DA�l�A�XA�E�A�bA���A���AܶFAܩ�Aܣ�Aܛ�AܓuA܉7A�~�A�v�A�l�A�dZA�^5A�K�A�9XA��A�  A���A�ZAڙ�Aٕ�A�S�A�C�A�-A��A�A��HA�ƨAؼjAذ!A؁A�"�A���A։7A�I�A�~�Aк^A�p�A���A�"�Aơ�A� �AÕ�A¥�A���A�O�A��+A�"�A�M�A�ȴA��A���A��A��RA�hsA�&�A�x�A�ȴA�oA���A�`BA��#A�K�A�|�A�dZA���A��#A�
=A���A�9XA��9A�ȴA���A���A���A�v�A�ƨA���A��\A�&�A�E�A�x�A���A��+A�z�A��A� �A�l�A~�+AzZAwt�At��AsG�ArAp�Am�FAi�wAc�^Aa33A`=qA_��A^�AY�mAV�\AS�AP(�AO��AN��AM��AM?}AK|�AG�hAGoAF�jAFAC��ABr�A@�yA>�A;A8�+A5|�A0jA/"�A-`BA,(�A+S�A*��A)�A(��A({A&�`A&$�A$��A#"�A" �A!+A v�Ap�A7LA�A�DAp�A�jA��A/A�\A��Ar�A��Ap�AXA?}AA�An�A�FAbA�A�Al�A��A�RA=qA�A	��A��AA�A�^A;dA�RAffAn�Ap�AĜA�TA�A�A�`A��A{A�^A��Ax�Ax�A�A��A+A�yAĜA��AĜAȴAĜAȴA��A�9A�A~�A^5A(�A;d@���@�&�@�bN@�1@�dZ@��H@���@�v�@��@��@���@��\@�v�@�n�@�ff@�J@�Q�@�  @�F@��@�!@�@�\@�~�@�-@�@��@�&�@�9X@���@���@�E�@��T@���@��^@�O�@��/@웦@�t�@��@���@�^@�x�@�@�o@�@�7@�hs@�O�@��@�bN@�S�@�^@�7L@���@���@�bN@�b@�33@�@�/@ܛ�@�z�@�b@�dZ@�"�@�@ڇ+@�hs@�r�@��@��H@ղ-@���@��@���@�ff@�-@�J@щ7@�V@�9X@�"�@�V@���@�7L@̴9@�9X@�  @���@˥�@�33@��@�J@�G�@�/@�1'@�\)@�;d@�V@�`B@��@���@��@���@ě�@�9X@�+@�E�@��T@�@�x�@�7L@���@���@�9X@���@���@�S�@�@�=q@���@��@��-@�x�@��j@�I�@�A�@�(�@� �@�1@��m@�;d@���@�5?@�@�G�@��/@�Ĝ@��@�Q�@� �@��m@�|�@�\)@�;d@��y@���@�E�@��@���@���@�`B@���@�z�@� �@�t�@�"�@��y@���@��@�p�@�/@���@���@���@�Z@�ƨ@��P@�dZ@�S�@�;d@���@��7@��@���@�Ĝ@�bN@��@��R@�=q@�{@���@���@�Z@�b@�C�@���@�{@�p�@�V@��9@�z�@�bN@�A�@���@���@��@�l�@��+@�hs@�%@�Q�@�ƨ@�;d@�ff@�J@��#@���@�@��-@���@���@���@��7@�hs@���@�Q�@��@���@��F@���@�|�@�l�@�\)@�"�@��H@��+@���@��#@��-@�p�@�O�@�?}@��@��`@��j@���@���@���@��D@�j@�9X@�b@���@��m@�ƨ@�l�@�;d@�
=@��+@��7@�O�@�7L@�7L@��@�%@�V@��@��@��@��@�&�@�V@��j@��u@�j@�1@�l�@�C�@�+@�o@�@��@���@���@�~�@�M�@���@�G�@��G�O�@��-@�n�@�n�@}��@p�9@j�@d�D@[�
@R�\@G�@<�@5p�@.ȴ@+33@%V@ �`@�-@�y@^5@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB<jB;dB;dB:^B9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB;dB;dB;dB;dB:^B9XB6FB49B/B#�B�B+B1B	7B1B1B1B+BBBB
��B
��B
�B
��B
�qB
�XB
�LB
�9B
�!B
�?B
�-B
�B
�'B
�!B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�uB
�JB
�%B
�B
z�B
u�B
m�B
ffB
cTB
`BB
n�B
hsB
p�B
�1B
l�B
��B
��B�B$�B-B)�B�B
��B
�B
��B
� B
t�B
m�B
y�B
hsB
/B	��B	�B	�ZB	��B	�FB	��B	�uB	�%B	}�B	s�B	aHB	F�B	,B	�B	�B	{B	PB��B�B�B�fB�`B�ZB�TB�NB�NB�NB�BB�;B�5B�NB�TB�ZB�ZB�yB�`B�B�
B�B�/B�BB�;B�;B�;B�/B�B�
B��B��BɺBƨBǮBƨBƨBƨBŢBŢBȴB��B��B��B��B�
B�B�B�B�B�B�
B��B��B�)B�ZB�fB�TB�#B��B��B��BȴB��B�qB�qB�wB��B��B��B��B��BɺB��B�;B�B��B	bB	oB	uB	�B	�B	�B	+B	6FB	9XB	8RB	8RB	8RB	@�B	K�B	M�B	S�B	ZB	\)B	^5B	]/B	\)B	[#B	XB	P�B	M�B	K�B	J�B	K�B	L�B	L�B	M�B	N�B	N�B	Q�B	Q�B	R�B	R�B	R�B	R�B	^5B	dZB	dZB	dZB	e`B	e`B	e`B	e`B	jB	l�B	o�B	v�B	{�B	}�B	�B	�1B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�9B	�3B	�9B	�?B	�?B	�LB	�^B	�jB	�qB	�}B	B	ÖB	ÖB	ÖB	B	ƨB	ȴB	ɺB	ɺB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�TB	�TB	�NB	�NB	�`B	�mB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
%B
bB
�B
�B
'�B
1'B
7LB
9XB
@�B
G�B
O�B
T�B
YB
\)B
`BB
dZB
gmB
m�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B<OB;HB;HB:AB9<B9;B9;B9;B:BB:BB:CB;GB;FB;JB;JB;JB;EB;JB:AB9=B6)B4B.�B#�BgBBB	BBBBBB�B�B
��B
��B
��B
��B
�WB
�>B
�2B
�B
�B
�$B
�B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�fB
�XB
�*B
�	B
��B
z�B
u�B
msB
fKB
c8B
`'B
n|B
hUB
p�B
�B
lkB
��B
��BrB$�B,�B)�B^B
��B
��B
��B
�B
t�B
mtB
y�B
hSB
.�B	��B	�B	�<B	��B	�*B	��B	�\B	�	B	}�B	s�B	a0B	F�B	+�B	�B	�B	dB	9B��B�B�nB�OB�HB�EB�=B�9B�9B�:B�-B�$B�B�9B�?B�EB�DB�bB�FB�B��B� B�B�)B�"B� B�!B�B�B��B��B��BɣBƐBǖBƎBƏBƐBňBňBțBʩB͹B��B��B��B��B��B��B��B��B��B��B��B�B�>B�JB�:B�B��BμB˭BȚB�oB�UB�WB�[B�hB�iB�oBˬB˪BɞB˫B�!B�uB��B	EB	QB	VB	uB	uB	�B	*�B	6&B	95B	82B	81B	82B	@cB	K�B	M�B	S�B	Y�B	\	B	^B	]B	\B	[B	W�B	P�B	M�B	K�B	J�B	K�B	L�B	L�B	M�B	N�B	N�B	Q�B	Q�B	R�B	R�B	R�B	R�B	^B	d8B	d7B	d8B	e:B	e>B	e;B	e>B	j\B	lkB	o}B	v�B	{�B	}�B	��B	�B	�!B	�3B	�KB	�\B	�\B	�\B	�hB	�vB	�tB	�oB	�tB	�pB	�\B	�JB	�jB	�oB	�{B	�|B	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�HB	�UB	�iB	�mB	�oB	�nB	�iB	�B	ȎB	ɔB	ɖB	ȍB	ǇB	ǈB	ǇB	ǈB	ǉB	ȌB	ʚB	ͫB	αB	βB	γB	αB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�	B	�B	�B	�B	�"B	�B	�B	�B	�$B	�-B	�2B	�2B	�,B	�+B	�&B	�#B	�8B	�FB	�<B	�<B	�>B	�:B	�EB	�DB	�DB	�EB	�CB	�DB	�GB	�=B	�EB	�QB	�UB	�cB	�]B	�ZB	�\B	�_B	�\B	�]B	�^B	�\B	�dB	�aB	�aB	�eB	�cB	�gB	�wB	�{B	�|B	�{B	�{B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
8B
WB
�B
'�B
0�B
7!B
9.B
@XB
G�B
O�B
T�B
X�B
[�B
`B
d-B
gAB
mdB
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451092016080714510920160807145109  AO  ARCAADJP                                                                    20150226221402    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221402  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221402  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145109  IP                  G�O�G�O�G�O�                