CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-08T02:15:58Z AOML 3.0 creation; 2016-08-07T21:51:16Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150408021558  20160807145116  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               -A   AO  5287_9017_045                   2C  D   APEX                            6529                            072314                          846 @�Gsu� 1   @�Gt`��@1�7KƧ��då�S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    -A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dyl�D��D�<�D��3D���D�  D�L�D�y�D�� D�fD�` D��3Dǹ�D���D�C3D�|�D�ɚD��D�9�D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�gD �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt|�Dy��D�+3D�K3D���D��3D�fD�[3D�� D��fD��D�nfD���D�� D�3D�Q�Dڋ3D�� D�3D�H D�t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�ZA�ZA�\)A�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�^5A�\)A�^5A�^5A�`BA�^5A�A�A�ȴA˥�A���A�ĜAʟ�A�5?A�v�A���AȓuA���AǶFAǗ�A�dZA�%A��A�{A�E�A�^5A�C�A��A��yA��
Aư!Aơ�A�~�A�M�A�;dA�;dA��A�Aŧ�Aŉ7A�l�A�XA�"�AĶFAĬA�n�A�A��A��A��A���A�1'A��;A��uA�1'A�A��A�ffA��A��mA�S�A��;A�p�A���A�+A�33A�`BA�E�A��A��A��A�-A�p�A��TA�A�A���A���A��7A��mA�v�A���A�Q�A�VA��A�VA���A�E�A��`A�A�v�A�ƨA���A���A���A��uA��A�Q�A�A���A�l�A�JA�?}A��A�\)A��HA��7A�\)A�`BA��A�7LA���A�ffA}XAxz�Au��AuXAs��Ap=qAmoAj�DAh��Ag�TAg%Ad�RAc`BAaXAY��AX^5AWl�AV  AT��AS/AP{AMO�AL9XAI|�AE�wADv�ADE�AC�FAB=qA@�yA>�A=A=VA<5?A9��A6��A6-A5oA2��A1;dA/\)A-��A+VA(5?A'�#A'��A&z�A$�!A$A"�\A n�AA�A{AbNA{A��A�/AƨA��A�A��A�
A{A(�A(�A(�AA�AffA�wA�hA��AhsAK�A;dA��AXA=qA&�A$�AhsA�PA��AVA
��A	��A	�TA�RAZA �A33A�A`BA{A�wA
=A�A�DAI�A$�A  A��A��A�A �@�K�@��@�@�G�@�b@��@�+@�v�@�9@�!@���@��@�&�@�@��y@��@�ƨ@���@��@�7@�/@��/@�/@�7@�O�@�x�@�t�@��@�@�K�@�hs@��@��;@�b@��@�V@���@�r�@�Q�@ݡ�@��@�p�@���@ؼj@�(�@��@�  @�  @�  @�K�@֏\@�?}@�  @�C�@��@���@�=q@�{@�E�@��@�t�@��/@���@�t�@��@ʇ+@�=q@ə�@�1@��T@Ł@��@��@��@�ȴ@�@�V@���@���@�V@��@��j@���@��@���@���@��@�t�@�t�@�t�@�|�@�|�@�;d@�33@�K�@�K�@���@�
=@�ȴ@�n�@�=q@�5?@�-@�=q@��#@��@�V@���@��m@�33@�33@��P@���@�1@��@��m@��F@��@��@���@�n�@�@���@���@�r�@��
@�\)@���@�$�@�=q@�ff@��@���@��@�X@�&�@���@��u@�I�@�1@�t�@��!@���@���@�G�@�V@��/@��u@�bN@�ƨ@���@�K�@�
=@�ȴ@�^5@�=q@��#@�%@��u@���@�"�@��y@�;d@��!@�^5@��H@��@��H@�ff@��T@�O�@�A�@�+@�;d@��@��@��-@�hs@�O�@�/@��@�V@��`@���@�z�@�I�@���@�K�@��y@�ȴ@��\@��@��@�{@��#@�G�@���@�G�@���@�1'@��;@��P@�"�@��y@���@�V@�-@�@��@��h@�&�@���@��@�I�@�b@��@�ƨ@���@�t�@�S�@��@�V@�{@��^@��7@��@��`@�Ĝ@�z�@�A�@��@��@�|�@��@��!@��\@�~�@�=q@�O�@���@�r�@�I�@�(�@�b@���@�ƨ@�K�@�
=@���@�M�@�{@���@���@��@�X@�/@���@��@�1'@��@���@���@�+@\)@sS�@j�H@a�^@Y7L@O
=@F��@A�7@;o@6$�@0�9@*-@$I�@�w@C�@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�ZA�ZA�ZA�\)A�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�^5A�\)A�^5A�^5A�`BA�^5A�A�A�ȴA˥�A���A�ĜAʟ�A�5?A�v�A���AȓuA���AǶFAǗ�A�dZA�%A��A�{A�E�A�^5A�C�A��A��yA��
Aư!Aơ�A�~�A�M�A�;dA�;dA��A�Aŧ�Aŉ7A�l�A�XA�"�AĶFAĬA�n�A�A��A��A��A���A�1'A��;A��uA�1'A�A��A�ffA��A��mA�S�A��;A�p�A���A�+A�33A�`BA�E�A��A��A��A�-A�p�A��TA�A�A���A���A��7A��mA�v�A���A�Q�A�VA��A�VA���A�E�A��`A�A�v�A�ƨA���A���A���A��uA��A�Q�A�A���A�l�A�JA�?}A��A�\)A��HA��7A�\)A�`BA��A�7LA���A�ffA}XAxz�Au��AuXAs��Ap=qAmoAj�DAh��Ag�TAg%Ad�RAc`BAaXAY��AX^5AWl�AV  AT��AS/AP{AMO�AL9XAI|�AE�wADv�ADE�AC�FAB=qA@�yA>�A=A=VA<5?A9��A6��A6-A5oA2��A1;dA/\)A-��A+VA(5?A'�#A'��A&z�A$�!A$A"�\A n�AA�A{AbNA{A��A�/AƨA��A�A��A�
A{A(�A(�A(�AA�AffA�wA�hA��AhsAK�A;dA��AXA=qA&�A$�AhsA�PA��AVA
��A	��A	�TA�RAZA �A33A�A`BA{A�wA
=A�A�DAI�A$�A  A��A��A�A �@�K�@��@�@�G�@�b@��@�+@�v�@�9@�!@���@��@�&�@�@��y@��@�ƨ@���@��@�7@�/@��/@�/@�7@�O�@�x�@�t�@��@�@�K�@�hs@��@��;@�b@��@�V@���@�r�@�Q�@ݡ�@��@�p�@���@ؼj@�(�@��@�  @�  @�  @�K�@֏\@�?}@�  @�C�@��@���@�=q@�{@�E�@��@�t�@��/@���@�t�@��@ʇ+@�=q@ə�@�1@��T@Ł@��@��@��@�ȴ@�@�V@���@���@�V@��@��j@���@��@���@���@��@�t�@�t�@�t�@�|�@�|�@�;d@�33@�K�@�K�@���@�
=@�ȴ@�n�@�=q@�5?@�-@�=q@��#@��@�V@���@��m@�33@�33@��P@���@�1@��@��m@��F@��@��@���@�n�@�@���@���@�r�@��
@�\)@���@�$�@�=q@�ff@��@���@��@�X@�&�@���@��u@�I�@�1@�t�@��!@���@���@�G�@�V@��/@��u@�bN@�ƨ@���@�K�@�
=@�ȴ@�^5@�=q@��#@�%@��u@���@�"�@��y@�;d@��!@�^5@��H@��@��H@�ff@��T@�O�@�A�@�+@�;d@��@��@��-@�hs@�O�@�/@��@�V@��`@���@�z�@�I�@���@�K�@��y@�ȴ@��\@��@��@�{@��#@�G�@���@�G�@���@�1'@��;@��P@�"�@��y@���@�V@�-@�@��@��h@�&�@���@��@�I�@�b@��@�ƨ@���@�t�@�S�@��@�V@�{@��^@��7@��@��`@�Ĝ@�z�@�A�@��@��@�|�@��@��!@��\@�~�@�=q@�O�@���@�r�@�I�@�(�@�b@���@�ƨ@�K�@�
=@���@�M�@�{@���@���@��@�X@�/@���@��@�1'G�O�@���@���@�+@\)@sS�@j�H@a�^@Y7L@O
=@F��@A�7@;o@6$�@0�9@*-@$I�@�w@C�@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�mB0!B�PB��B�BĜB��BoB�B�BbB\B�B!�B%�B7LBQ�By�B��B��B��B��B�B�B�B�!B�FB�RB�XB�^B�wB�}B��BBĜB��B�)B�5B�5B�ZB�B\B-B0!B49B@�BO�B[#BYB[#B`BBgmBjBq�Br�Bq�BiyB^5BP�B>wB6FB33B33B7LBD�BG�B>wBJ�BF�B<jB+B �B�BPBB��B�B�fB�#B��B��B�-B��Bl�B]/BN�B@�B.B�B1B�B�B�BŢB�B�VBx�B_;B=qBB
�fB
ȴB
��B
`BB
G�B
\B	�B	�#B	�B	��B	�FB	��B	�bB	�+B	� B	y�B	k�B	aHB	S�B	49B	,B	&�B	 �B	�B	oB	1B��B��B�B�B�yB�sB�fB�HB�#B��B��B��B��B��B�
B�B�B��B��B�B�mB�TB�/B�5B�ZB�yB�yB�ZB�HB�
B�B��B��B�
B�B�TB�fB�HB�yB��B	DB	hB	uB	uB	�B	.B	/B	2-B	33B	2-B	2-B	2-B	1'B	1'B	49B	0!B	+B	'�B	$�B	&�B	"�B	�B	�B	�B	�B	�B	 �B	49B	8RB	?}B	@�B	=qB	<jB	L�B	M�B	S�B	]/B	cTB	ffB	gmB	iyB	n�B	� B	� B	r�B	[#B	B�B	7LB	1'B	49B	/B	5?B	7LB	33B	5?B	9XB	@�B	I�B	P�B	W
B	[#B	aHB	dZB	dZB	cTB	e`B	jB	s�B	s�B	y�B	v�B	q�B	n�B	m�B	hsB	ffB	dZB	hsB	n�B	p�B	gmB	dZB	jB	y�B	v�B	r�B	s�B	s�B	t�B	u�B	w�B	x�B	z�B	}�B	� B	�B	�%B	�+B	�+B	�+B	�B	�+B	�7B	�=B	�1B	�B	�B	�B	�B	�%B	�B	�B	� B	� B	�B	�1B	�+B	�B	�+B	�+B	�%B	�%B	�1B	�7B	�=B	�7B	�=B	�=B	�=B	�=B	�DB	�DB	�PB	�PB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�RB	�^B	�dB	�dB	�jB	�wB	�}B	�}B	�}B	�qB	�qB	�qB	��B	��B	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�TB	�HB	�5B	�)B	�B	�B	�B	�#B	�#B	�)B	�BB	�`B	�mB	�mB	�mB	�fB	�ZB	�TB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B
oB
�B
�B
+B
1'B
6FB
;dB
C�B
J�B
Q�B
W
B
[#B
^5B
cTB
gmB
jB
o�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B
ͱB
ͱB
ͰB
ͰB
ͱB
̩B
ͯB
ͰB
ͳB
ͳB
ͰB
ͰB
ͱB
ͰB
ͱB
ͱB
ͳB
ͱB
�MB/�B�,B��B��B�xB��BJBuB�B=B5BoB!�B%�B7%BQ�By�B�wB��B��B��B��B��B��B��B�#B�/B�2B�:B�SB�XB�eB�hB�{B˥B�B�B�B�7B�B9B,�B/�B4B@_BO�BZ�BX�B[ B`BgLBj]Bq�Br�Bq�BiUB^BP�B>TB6#B3B3B7)BDxBG�B>SBJ�BF�B<DB*�B �BtB+B�B��B��B�?B��B��B˞B�B�cBl]B]BN�B@YB-�BBB�B�[B��B�xB��B�*Bx�B_B=JB�B
�=B
ȈB
�eB
`B
G�B
8B	�gB	��B	��B	̦B	� B	��B	�?B	�B	�B	y�B	kcB	a%B	S�B	4B	+�B	&�B	 �B	B	NB	B��B��B�~B�lB�XB�TB�FB�)B�B��B��B��B��B��B��B��B��B��B��B��B�KB�2B�B�B�8B�WB�YB�8B�%B��B��B��B��B��B��B�1B�CB�#B�VB��B	B	BB	OB	MB	vB	-�B	.�B	2B	3B	2B	2B	2B	0�B	0�B	4B	/�B	*�B	'�B	$�B	&�B	"�B	�B	�B	~B	zB	sB	 �B	4B	8)B	?SB	@XB	=IB	<AB	L�B	M�B	S�B	]B	c+B	f;B	gCB	iNB	nlB	�B	�B	r�B	Z�B	BfB	7#B	0�B	4B	.�B	5B	7#B	3B	5B	9,B	@ZB	I�B	P�B	V�B	Z�B	aB	d.B	d0B	c&B	e4B	jSB	s�B	s�B	y�B	v�B	qzB	njB	mfB	hFB	f6B	d-B	hEB	nhB	pwB	g?B	d,B	jQB	y�B	v�B	r�B	s�B	s�B	t�B	u�B	w�B	x�B	z�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�&B	�'B	�4B	�WB	�XB	�XB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�,B	�3B	�3B	�9B	�HB	�LB	�IB	�KB	�@B	�<B	�?B	�SB	�RB	�ZB	�kB	ɉB	˕B	̜B	̚B	̙B	̜B	͡B	͠B	ϫB	гB	ѺB	��B	ѺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�"B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�.B	�:B	�:B	�:B	�4B	�(B	�!B	�)B	�/B	�(B	�)B	�&B	�)B	�&B	�/B	�(B	�(B	�&B	�'B	�'B	�1B	�3B	�:B	�;B	�=B	�EB	�KB	�WB	�^B	�XB	�WB	�vB	�qB	�jB	�oB	�xB	�vB	�tB	�xB	�uB	�|B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
	B
:B
xB
�B
*�B
0�B
6B
;.B
CaB
J�B
Q�B
V�B
Z�B
]�B
cB
g6B
jGB
ogB
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451162016080714511620160807145116  AO  ARCAADJP                                                                    20150408021558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150408021558  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150408021558  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145116  IP                  G�O�G�O�G�O�                