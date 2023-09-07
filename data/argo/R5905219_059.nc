CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2019-12-02T21:37:36Z creation;2019-12-02T21:37:39Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191202213736  20191202215344  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ;A   JA                                  2B  A   APEX                            7906                            051216                          846 @��r�io1   @��s8�9@2��$�/�d�n��P1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  BxffB��B�  B���B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C%�fC(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Df��Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dw��Dxy�Dy  Dy� Dy��Dz� D{  D{� D|  D|� D}  D}� D~  D~� D~��D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D���D�<�D�� D�� D���D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�C3D�� D��D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D�3D�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�B@BI(�BQ(�BY(�Ba(�Bi(�Bq(�By�\B�aHB��{B�aHB��{B��{B��{B�ǮB�ǮB��{B��{B��{B�aHB��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�aHB��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C 0�C"J=C$J=C&0�C(J=C*J=C,J=C.J=C00�C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=Cjc�ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D�)D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#)D#�)D$�D$��D%�D%��D&�D&��D')D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4)D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH)DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`)D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df)Df�)Dg)Dg�)Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm)Dm�)Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx)Dx�)Dy�Dy��Dz)Dz��D{�D{��D|�D|��D}�D}��D~�D~��D)D��D�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��{D��HD�	HD�IHD��D��D�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�L{D��{D��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�{D�IHD��HD��HD�D�FD��HD��HD�D�IHD��HD��HD�{D�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��D��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��D�D�FD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�FD��D��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�L{D��HD��D�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��D��D�	HD�IHD��D��HD�	HD�IHD��HD��HD�	HD�IHD��HD��{D�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHDHD��HD�	HD�IHDÉHD��HD�	HD�IHDĉHD��HD�	HD�IHDŉHD��HD�	HD�IHDƉHD��HD�	HD�IHDǉHD��HD�	HD�IHDȉHD��HD�	HD�IHDɉHD��HD�	HD�IHDʉHD��HD�	HD�IHDˉHD��HD�D�IHD̉HD��HD�	HD�IHD͉HD��HD�	HD�IHDΉHD��HD�	HD�IHDωHD��HD�	HD�IHDЉHD��HD�	HD�IHDщHD��HD�	HD�IHD҉HD��HD�	HD�IHDӉHD��HD�	HD�IHDԉHD��HD�	HD�IHDՉHD��HD�	HD�IHD։HD��HD�	HD�IHD׉HD��HD�	HD�IHD؉HD��HD�	HD�IHDىHD��HD�	HD�IHDډHD��HD�	HD�IHDۉHD��HD�	HD�IHD܉HD��HD�	HD�IHD݉HD��HD�	HD�IHDމHD��HD�	HD�IHD߉HD��HD�	HD�L{D��HD��D�	HD�L{D�{D��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�D��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�D��HD�	HD�IHD��HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�D�IHD��HD��{D�{D�YH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AۑhAۏ\Aۏ\Aۡ�Aۣ�Aۣ�Aۥ�Aۛ�AۑhAۓuAۗ�Aە�Aە�Aۗ�Aۙ�Aۙ�Aۙ�Aۙ�A۝�A�ĜA�ƨA��/A�A��A��A��A��#A۶FA�;dA���A�I�AсA���A͛�AˬAʃA��#A��/AƬAũ�A�7LA�ĜA�VAÃA�E�A���A��TA�/A�p�A��uA�S�A��A�?}A���A��RA���A��RA�dZA�(�A�C�A���A�~�A��^A��A��A�JA� �A�JA�ĜA��A�S�A�Q�A�l�A��A�r�A�VA�x�A�oA�ĜA�XA��A�/A�~�A�oA���A���A��hA�5?A���A�x�A��#A���A���A��uA�5?A�oA��A�A���A�|�A�JA���A��A�G�A�
=A��\A��A�r�A�I�A�ĜA�K�A�~�A~��A}dZA{�PAzr�AyK�Ax��Aw;dAtv�Aq�hAnM�An$�Al-Ai�Ai?}Ah�`Agp�AfE�Ae�Ac��AbZA`{A\�uAZn�AX-AU�;AU
=AS��ASoAPĜAN�jAN�AMhsALJAJ��AI`BAG?}AF��AF^5AD��AA�A>jA;�mA;7LA9�;A6r�A4ĜA4I�A3��A2ffA0Q�A,A�A+;dA*$�A)�^A)"�A(��A(1A'��A'S�A%�A#�A"��A!�A!S�A �RA =qA�A�/AA�A/A�mA��A=qA�`A�AbNAS�A�\A�AoA1'A�FAr�A/AZAA�mA�^A�`A
�HA�\A�/At�AM�A5?AC�A�FA ��A �@��!@�$�@��@�1'@�;d@��!@�$�@��7@���@���@�@�t�@���@�(�@�@�v�@�G�@�1'@�z�@웦@�@�D@���@�;d@�x�@�|�@�|�@�33@�;d@��@�Z@�1@��@�-@�1'@�=q@��`@�"�@���@�C�@�C�@�\)@�K�@���@�p�@؛�@֟�@���@�7L@�/@�G�@ղ-@�X@�=q@���@�V@��@��`@ѡ�@���@���@�I�@�I�@Ӯ@�ȴ@���@�7L@�Z@��;@ύP@�ȴ@�@��@�(�@��
@ʸR@�J@���@�/@��@��H@Ƨ�@�v�@�x�@�G�@�X@�p�@�?}@�(�@°!@��@�M�@��-@�x�@�?}@��@�r�@�1@�b@��
@�@�C�@�o@���@�v�@�-@���@�`B@��@��P@�=q@��@��@��@��9@�z�@�(�@��@��@��\@�~�@��\@���@���@�p�@�X@�G�@�%@�Q�@��@��m@��;@��@�|�@�|�@�\)@�+@���@�=q@��@��@�r�@�A�@���@�j@�l�@��@�~�@�v�@�~�@�^5@��@�x�@���@�r�@�1@��
@��@�;d@�o@���@��+@�V@�5?@��@��@�p�@�?}@���@���@�bN@� �@��P@���@��@��+@�-@�@�G�@��@�Ĝ@�z�@�9X@���@�C�@���@�-@���@�hs@�O�@�hs@�`B@�p�@�X@�%@��j@��T@��-@��7@�X@�%@���@�z�@��@�33@���@���@�J@���@��
@�\)@�"�@���@�-@���@��-@���@��7@�7L@���@�r�@�1'@�1'@�b@���@��;@��
@���@���@�|�@�S�@�S�@�C�@�|�@�t�@�S�@���@�|�@�S�@�C�@�o@�ȴ@���@�~�@��T@�G�@��`@���@���@�j@�b@��@���@�@��H@�-@���@�p�@���@��u@�j@�I�@�b@�1@�1@���@��;@��w@�\)@��@�
=@��y@��\@�J@��#@��-@�hs@�V@��/@���@��D@�r�@�b@��
@�l�@��@��@�$�@��h@�p�@�G�@���@�Ĝ@���@�1'@��@���@���@��P@�t�@�dZ@�\)@�K�@�+@��@���@���@���@�n�@�=q@�J@���@�x�@�?}@�V@��u@�j@��@��;@���@�K�@�C�@��@���@�~�@�V@�-@���@��-@�p�@��@���@���@��9@�Q�@�1'@� �@�1@��@�@~v�@~5?@}��@}?}@}?}@}V@|��@|�@{��@{�@{o@z��@z-@y��@y7L@xA�@w\)@v�@v��@vv�@vV@v@u�@t��@t�D@tI�@t1@s��@s@qx�@q&�@p��@p��@p�u@p�@pr�@pbN@pA�@p �@p  @o�;@o�@o�P@o\)@o�@o
=@n��@n�@n��@nV@m�T@l��@k�
@k33@j��@j�\@j�@i��@ix�@iG�@iG�@i7L@h��@h�9@hA�@g��@gK�@f�R@e@d��@d�j@dz�@c�
@b��@b�@a�#@a��@ax�@a�@`��@`�9@`bN@_��@_+@^E�@]p�@\�@\1@[��@[33@Z�@Z�H@Z��@Z~�@ZM�@ZJ@Y�@Y��@X��@XA�@X  @W�;@W��@W��@W�w@W�w@W�@Vȴ@V�+@VE�@V$�@U@U/@UV@T��@T��@Tz�@T�@S�
@S�@SC�@SC�@R�H@R�\@Q�@QG�@Q7L@Q&�@Q�@Q%@P��@Pr�@O�;@O|�@O\)@O;d@O�@N�y@N��@N5?@N@M@M�-@M�@M�@MV@L��@L�@L�@L�@K�
@Ko@J��@J��@Jn�@JM�@J-@JJ@I�^@Ihs@IG�@I�@Hr�@G�;@G�w@G�@G�P@GK�@F�y@F��@FE�@F@E��@E�h@E?}@D�D@C��@C�F@CdZ@C33@B�@BJ@A�#@A��@@��@@��@@�@@�@@bN@?�;@?|�@?;d@>ȴ@>E�@>$�@=@<�/@<�@<z�@;�m@;��@;dZ@;o@:�\@9�#@9G�@8��@8�`@8��@8��@8�@7l�@6�y@6ȴ@6ȴ@6�R@6�+@6{@6@5��@5�-@5�h@5�@5`B@5O�@5�@5�@4��@49X@4�@3��@3@2��@2~�@2-@1hs@0�9@0 �@/��@/�w@/|�@/+@.�R@.E�@-�@-@-`B@,��@,��@,z�@,I�@,9X@,1@+�
@+�@*�@*��@*�\@*M�@)�7@)�@(��@(r�@(A�@( �@'��@'|�@';d@&�y@&ȴ@&v�@%�@%�-@%��@%�@%O�@$��@$I�@$9X@$�@#ƨ@#��@#dZ@#o@"��@"�\@"M�@"J@!��@!�^@!��@ ��@ ��@ �9@ ��@ �u@ r�@ Q�@ A�@ A�@ 1'@ b@|�@��@�R@ff@$�@@��@�h@p�@?}@�@�@�D@1@�F@��@33@�@��@��@�!@��@�\@^5@�@J@J@�@��@��@��@�7@�@bN@Q�@A�@A�@1'@�@\)@�R@E�@�@��@�-@�-@�-@�-@��@p�@?}@�@9X@(�@(�@(�@�@�
@dZ@33@�@�H@�!@M�@��@x�@hs@X@G�@&�@%@�`@�u@A�@ �@�@��@�P@�P@|�@l�@l�@\)@;d@��@�@��@V@E�@��@��@�@`B@O�@/@V@��@�@��@Z@�@�m@�F@dZ@S�@C�@33@o@
�@
�H@
�H@
n�@
J@	��@	�^@	x�@	%@��@��@�u@r�@Q�@b@  @�;@��@|�@l�@\)@\)@;d@;d@�@�@��@E�@�T@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AۑhAۏ\Aۏ\Aۡ�Aۣ�Aۣ�Aۥ�Aۛ�AۑhAۓuAۗ�Aە�Aە�Aۗ�Aۙ�Aۙ�Aۙ�Aۙ�A۝�A�ĜA�ƨA��/A�A��A��A��A��#A۶FA�;dA���A�I�AсA���A͛�AˬAʃA��#A��/AƬAũ�A�7LA�ĜA�VAÃA�E�A���A��TA�/A�p�A��uA�S�A��A�?}A���A��RA���A��RA�dZA�(�A�C�A���A�~�A��^A��A��A�JA� �A�JA�ĜA��A�S�A�Q�A�l�A��A�r�A�VA�x�A�oA�ĜA�XA��A�/A�~�A�oA���A���A��hA�5?A���A�x�A��#A���A���A��uA�5?A�oA��A�A���A�|�A�JA���A��A�G�A�
=A��\A��A�r�A�I�A�ĜA�K�A�~�A~��A}dZA{�PAzr�AyK�Ax��Aw;dAtv�Aq�hAnM�An$�Al-Ai�Ai?}Ah�`Agp�AfE�Ae�Ac��AbZA`{A\�uAZn�AX-AU�;AU
=AS��ASoAPĜAN�jAN�AMhsALJAJ��AI`BAG?}AF��AF^5AD��AA�A>jA;�mA;7LA9�;A6r�A4ĜA4I�A3��A2ffA0Q�A,A�A+;dA*$�A)�^A)"�A(��A(1A'��A'S�A%�A#�A"��A!�A!S�A �RA =qA�A�/AA�A/A�mA��A=qA�`A�AbNAS�A�\A�AoA1'A�FAr�A/AZAA�mA�^A�`A
�HA�\A�/At�AM�A5?AC�A�FA ��A �@��!@�$�@��@�1'@�;d@��!@�$�@��7@���@���@�@�t�@���@�(�@�@�v�@�G�@�1'@�z�@웦@�@�D@���@�;d@�x�@�|�@�|�@�33@�;d@��@�Z@�1@��@�-@�1'@�=q@��`@�"�@���@�C�@�C�@�\)@�K�@���@�p�@؛�@֟�@���@�7L@�/@�G�@ղ-@�X@�=q@���@�V@��@��`@ѡ�@���@���@�I�@�I�@Ӯ@�ȴ@���@�7L@�Z@��;@ύP@�ȴ@�@��@�(�@��
@ʸR@�J@���@�/@��@��H@Ƨ�@�v�@�x�@�G�@�X@�p�@�?}@�(�@°!@��@�M�@��-@�x�@�?}@��@�r�@�1@�b@��
@�@�C�@�o@���@�v�@�-@���@�`B@��@��P@�=q@��@��@��@��9@�z�@�(�@��@��@��\@�~�@��\@���@���@�p�@�X@�G�@�%@�Q�@��@��m@��;@��@�|�@�|�@�\)@�+@���@�=q@��@��@�r�@�A�@���@�j@�l�@��@�~�@�v�@�~�@�^5@��@�x�@���@�r�@�1@��
@��@�;d@�o@���@��+@�V@�5?@��@��@�p�@�?}@���@���@�bN@� �@��P@���@��@��+@�-@�@�G�@��@�Ĝ@�z�@�9X@���@�C�@���@�-@���@�hs@�O�@�hs@�`B@�p�@�X@�%@��j@��T@��-@��7@�X@�%@���@�z�@��@�33@���@���@�J@���@��
@�\)@�"�@���@�-@���@��-@���@��7@�7L@���@�r�@�1'@�1'@�b@���@��;@��
@���@���@�|�@�S�@�S�@�C�@�|�@�t�@�S�@���@�|�@�S�@�C�@�o@�ȴ@���@�~�@��T@�G�@��`@���@���@�j@�b@��@���@�@��H@�-@���@�p�@���@��u@�j@�I�@�b@�1@�1@���@��;@��w@�\)@��@�
=@��y@��\@�J@��#@��-@�hs@�V@��/@���@��D@�r�@�b@��
@�l�@��@��@�$�@��h@�p�@�G�@���@�Ĝ@���@�1'@��@���@���@��P@�t�@�dZ@�\)@�K�@�+@��@���@���@���@�n�@�=q@�J@���@�x�@�?}@�V@��u@�j@��@��;@���@�K�@�C�@��@���@�~�@�V@�-@���@��-@�p�@��@���@���@��9@�Q�@�1'@� �@�1@��@�@~v�@~5?@}��@}?}@}?}@}V@|��@|�@{��@{�@{o@z��@z-@y��@y7L@xA�@w\)@v�@v��@vv�@vV@v@u�@t��@t�D@tI�@t1@s��@s@qx�@q&�@p��@p��@p�u@p�@pr�@pbN@pA�@p �@p  @o�;@o�@o�P@o\)@o�@o
=@n��@n�@n��@nV@m�T@l��@k�
@k33@j��@j�\@j�@i��@ix�@iG�@iG�@i7L@h��@h�9@hA�@g��@gK�@f�R@e@d��@d�j@dz�@c�
@b��@b�@a�#@a��@ax�@a�@`��@`�9@`bN@_��@_+@^E�@]p�@\�@\1@[��@[33@Z�@Z�H@Z��@Z~�@ZM�@ZJ@Y�@Y��@X��@XA�@X  @W�;@W��@W��@W�w@W�w@W�@Vȴ@V�+@VE�@V$�@U@U/@UV@T��@T��@Tz�@T�@S�
@S�@SC�@SC�@R�H@R�\@Q�@QG�@Q7L@Q&�@Q�@Q%@P��@Pr�@O�;@O|�@O\)@O;d@O�@N�y@N��@N5?@N@M@M�-@M�@M�@MV@L��@L�@L�@L�@K�
@Ko@J��@J��@Jn�@JM�@J-@JJ@I�^@Ihs@IG�@I�@Hr�@G�;@G�w@G�@G�P@GK�@F�y@F��@FE�@F@E��@E�h@E?}@D�D@C��@C�F@CdZ@C33@B�@BJ@A�#@A��@@��@@��@@�@@�@@bN@?�;@?|�@?;d@>ȴ@>E�@>$�@=@<�/@<�@<z�@;�m@;��@;dZ@;o@:�\@9�#@9G�@8��@8�`@8��@8��@8�@7l�@6�y@6ȴ@6ȴ@6�R@6�+@6{@6@5��@5�-@5�h@5�@5`B@5O�@5�@5�@4��@49X@4�@3��@3@2��@2~�@2-@1hs@0�9@0 �@/��@/�w@/|�@/+@.�R@.E�@-�@-@-`B@,��@,��@,z�@,I�@,9X@,1@+�
@+�@*�@*��@*�\@*M�@)�7@)�@(��@(r�@(A�@( �@'��@'|�@';d@&�y@&ȴ@&v�@%�@%�-@%��@%�@%O�@$��@$I�@$9X@$�@#ƨ@#��@#dZ@#o@"��@"�\@"M�@"J@!��@!�^@!��@ ��@ ��@ �9@ ��@ �u@ r�@ Q�@ A�@ A�@ 1'@ b@|�@��@�R@ff@$�@@��@�h@p�@?}@�@�@�D@1@�F@��@33@�@��@��@�!@��@�\@^5@�@J@J@�@��@��@��@�7@�@bN@Q�@A�@A�@1'@�@\)@�R@E�@�@��@�-@�-@�-@�-@��@p�@?}@�@9X@(�@(�@(�@�@�
@dZ@33@�@�H@�!@M�@��@x�@hs@X@G�@&�@%@�`@�u@A�@ �@�@��@�P@�P@|�@l�@l�@\)@;d@��@�@��@V@E�@��@��@�@`B@O�@/@V@��@�@��@Z@�@�m@�F@dZ@S�@C�@33@o@
�@
�H@
�H@
n�@
J@	��@	�^@	x�@	%@��@��@�u@r�@Q�@b@  @�;@��@|�@l�@\)@\)@;d@;d@�@�@��@E�@�T@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�DB
�DB
�DB
�JB
�PB
�PB
�PB
�DB
�1B
�1B
�7B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
��B
��B
��B
�jB
��B
�5B
�mB
��B
��BhBP�B��B~�B� Bw�Bo�Bn�B�%B�1B�%B� B� B~�B~�B�JB�+B�B�B�B�7B�VB�PB�VB�\B�bB�\B�VB�\B�PB�PB�JB�1B�1B�+Bp�Bl�BhsB_;BYB]/BXBVBM�B?}B5?B+B%�B�B�BhB��B�B��BŢB��B��B�}B�dB�3B�B�B��B�uB�+By�Br�Bp�Bm�Bm�BjBhsBaHB[#BS�BH�B49B,B�B
�B
��B
ɺB
��B
�B
l�B
hsB
\)B
T�B
N�B
J�B
A�B
1'B
!�B

=B
	7B
B	�B	�B	�sB	�NB	�B	��B	ɺB	��B	�9B	��B	�oB	�=B	u�B	q�B	gmB	bNB	ZB	L�B	I�B	F�B	A�B	<jB	6FB	0!B	-B	)�B	"�B	�B		7B��B��B�B�yB�BB�)B�B�B��BǮBB�}B�qB�dB�^B�RB�FB�FB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�oB�oB�\B�\B�VB�PB�7B�=B�+B�B�B�B�B~�Bz�B|�B{�B{�Bw�Bw�Bw�Bw�Bv�Bv�Bu�Bu�Bv�Bw�Bx�Bw�Bw�Bw�Bx�Bz�B|�B|�B|�B}�B}�B}�B� B�B�B�+B�1B�PB�uB�bB�bB��B��B�B�!B�?B�LB�qB�jB�qB�jB�9B��B��B��B��B��B��B��B��B�
B�B�B��B�5B�ZB�fB�B	%B	�B	VB		7B	B	B	hB	�B	/B	5?B	7LB	;dB	=qB	>wB	@�B	C�B	E�B	F�B	H�B	J�B	L�B	M�B	N�B	O�B	O�B	N�B	P�B	VB	YB	ZB	ZB	_;B	_;B	_;B	`BB	aHB	dZB	bNB	e`B	ffB	e`B	dZB	ffB	jB	l�B	l�B	n�B	o�B	o�B	r�B	s�B	t�B	t�B	u�B	x�B	x�B	v�B	v�B	u�B	w�B	{�B	�B	�B	�B	�B	�B	�7B	�DB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�9B	�FB	�RB	�XB	�jB	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�5B	�;B	�BB	�BB	�NB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
JB
VB
hB
oB
oB
oB
uB
{B
uB
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
"�B
#�B
%�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
)�B
+B
)�B
+B
+B
+B
,B
,B
-B
.B
.B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
W
B
VB
W
B
W
B
W
B
W
B
XB
XB
XB
W
B
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�DB
�DB
�DB
�JB
�PB
�PB
�PB
�DB
�1B
�1B
�7B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
��B
��B
��B
�jB
��B
�5B
�mB
��B
��BhBP�B��B~�B� Bw�Bo�Bn�B�%B�1B�%B� B� B~�B~�B�JB�+B�B�B�B�7B�VB�PB�VB�\B�bB�\B�VB�\B�PB�PB�JB�1B�1B�+Bp�Bl�BhsB_;BYB]/BXBVBM�B?}B5?B+B%�B�B�BhB��B�B��BŢB��B��B�}B�dB�3B�B�B��B�uB�+By�Br�Bp�Bm�Bm�BjBhsBaHB[#BS�BH�B49B,B�B
�B
��B
ɺB
��B
�B
l�B
hsB
\)B
T�B
N�B
J�B
A�B
1'B
!�B

=B
	7B
B	�B	�B	�sB	�NB	�B	��B	ɺB	��B	�9B	��B	�oB	�=B	u�B	q�B	gmB	bNB	ZB	L�B	I�B	F�B	A�B	<jB	6FB	0!B	-B	)�B	"�B	�B		7B��B��B�B�yB�BB�)B�B�B��BǮBB�}B�qB�dB�^B�RB�FB�FB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�oB�oB�\B�\B�VB�PB�7B�=B�+B�B�B�B�B~�Bz�B|�B{�B{�Bw�Bw�Bw�Bw�Bv�Bv�Bu�Bu�Bv�Bw�Bx�Bw�Bw�Bw�Bx�Bz�B|�B|�B|�B}�B}�B}�B� B�B�B�+B�1B�PB�uB�bB�bB��B��B�B�!B�?B�LB�qB�jB�qB�jB�9B��B��B��B��B��B��B��B��B�
B�B�B��B�5B�ZB�fB�B	%B	�B	VB		7B	B	B	hB	�B	/B	5?B	7LB	;dB	=qB	>wB	@�B	C�B	E�B	F�B	H�B	J�B	L�B	M�B	N�B	O�B	O�B	N�B	P�B	VB	YB	ZB	ZB	_;B	_;B	_;B	`BB	aHB	dZB	bNB	e`B	ffB	e`B	dZB	ffB	jB	l�B	l�B	n�B	o�B	o�B	r�B	s�B	t�B	t�B	u�B	x�B	x�B	v�B	v�B	u�B	w�B	{�B	�B	�B	�B	�B	�B	�7B	�DB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�9B	�FB	�RB	�XB	�jB	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�5B	�;B	�BB	�BB	�NB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
JB
VB
hB
oB
oB
oB
uB
{B
uB
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
"�B
#�B
%�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
)�B
+B
)�B
+B
+B
+B
,B
,B
-B
.B
.B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
W
B
VB
W
B
W
B
W
B
W
B
XB
XB
XB
W
B
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20191203063713  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191202213736  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191202213737  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191202213737  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191202213738  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191202213738  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191202213738  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191202213738  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191202213738  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191202213739                      G�O�G�O�G�O�                JA  ARUP                                                                        20191202215344                      G�O�G�O�G�O�                