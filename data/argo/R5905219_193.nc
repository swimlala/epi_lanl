CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-09T21:43:24Z creation;2023-07-09T21:43:25Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20230709214324  20230709215711  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�9,���1   @�9,���@4�Z�1�c��hr�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B ffB33B  B  B   B(  B0  B8ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"y�D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�<�D� D�� D�3D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D��D���D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @;�@�@�A�HA&�HAF�HAf�HA�=qA�p�A�p�A�p�A�p�A�p�A�p�A�p�B�B�B�RB�RB!�RB)�RB1�RB:�BAQ�BI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B̨�BШ�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C ��CnCnCnCnC
��C��CnCnCnCnCnCnCnCnCnC nC"TzC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCD��CF��CHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\��C^��C`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�C�C�C�C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D �D ��D�D��D!�D��D�D��D�D��D�D��D�D��D�D��D�D��D	!�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D �D ��D!�D!��D"�D"�D#D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN�DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp!�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�J�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�J�D���D�ʐD�
�D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D�ʐD�
�D�J�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�J�D���D���D��D�M�D���D���D��D�M�D���D���D��D�J�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D�D���D��D�M�DÍ�D���D��D�P�Dč�D���D��D�M�Dō�D���D��D�M�Dƍ�D���D��D�M�DǍ�D���D��D�M�Dȍ�D���D��D�M�Dɍ�D���D��D�M�Dʍ�D���D�
�D�M�Dˍ�D���D��D�M�D̍�D���D��D�M�D͍�D���D��D�M�D΍�D���D��D�M�Dύ�D���D��D�M�DЍ�D���D��D�M�Dэ�D���D��D�M�Dҍ�D���D��D�J�DӍ�D���D��D�M�Dԍ�D���D��D�M�DՍ�D���D��D�M�D֍�D���D��D�M�D׍�D���D��D�M�D؍�D���D��D�M�Dٍ�D���D��D�M�Dڍ�D���D��D�M�Dۍ�D���D��D�M�D܍�D���D��D�M�Dݍ�D���D��D�M�Dލ�D���D��D�M�Dߍ�D���D��D�M�D���D���D�
�D�J�D��D���D��D�M�D��D���D�
�D�J�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D�ʐD�
�D�M�D��D���D��D�M�D��D���D��D�M�D��D�ʐD�
�D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�G]D�4)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�9XA�9XA�7LA�5?A�;dA�9XA�9XA�;dA�;dA�?}A�A�A�?}A�;dA�9XA��A�-AυA��mA���AȺ^AǛ�A�33A���A�E�A�A�A�A�n�A�{A���A�Q�A��FA�1A���A��yA�ZA��`A���A�t�A���A�O�A�n�A��A��7A���A�^5A�JA�`BA��PA��
A��A�A���A�G�A���A��jA�5?A��!A�oA�z�A���A��`A�XA�"�A��\A���A�"�A�C�A�K�A��uA��yA��\A�=qA�dZA�Q�A�/A��A�A�A��9A�p�A��PA�E�A�A�A�XA�"�A���A���A�O�A��Ap�A~$�A|�!Az5?Ay7LAxbAwt�Av$�AtQ�Ar�AoS�Amt�AkƨAg"�Adr�Act�AbȴA`�jA^1'AY�
AW��AV(�AS�ARM�AP~�AM�^AK�mAJ�!AI��AH �AG�^AG��AG`BAE��AC�TAC`BAB$�A?�;A>$�A<�yA;��A9��A8(�A6r�A5VA3\)A1O�A.��A-�#A,ffA++A(�A&ȴA%A$�yA$�uA#�#A"��A"��A"v�A!|�A Q�AS�AȴA�AO�A��A^5A��A33A�yA��A=qA�A�A��A�!AA�A{AA��A�`AbA��A33A%A��A�A��A33AM�A��AA	p�AĜA��AI�A��Ap�A33AVA��A1A�FA�PA"�AhsAJA33A�A z�@�t�@�ȴ@���@���@��@���@�`B@�/@��@�Z@�+@��@�%@�Z@��
@�@�C�@�-@�b@�V@�x�@�7@웦@��@��#@�^@�x�@�33@��`@�D@��;@�C�@�~�@�-@�@�/@�1@�l�@�M�@ݑh@�X@���@ڏ\@��m@���@�+@ҟ�@�M�@�x�@мj@� �@ΰ!@�J@��T@͙�@��@̓u@�+@ʏ\@ɲ-@ȓu@���@Ǿw@�t�@���@Ƨ�@�v�@�J@���@ź^@�O�@��@�Ĝ@�r�@�  @Õ�@���@�-@��^@���@���@�p�@��@�r�@��@�\)@�ff@�@���@��/@��@��u@��@�l�@��!@�V@���@��-@��u@�33@�=q@�`B@�1'@��
@���@��\@�M�@�@���@���@�x�@�`B@�7L@���@��u@�bN@�Q�@�(�@���@�\)@�{@��^@���@�?}@�&�@��j@��@�ȴ@��@��@�@�o@�33@�C�@��@��!@�V@�J@��@��@�A�@���@���@���@�t�@�C�@���@�v�@�=q@���@��h@��h@��7@�G�@��`@���@�z�@�A�@�1@��;@�\)@��@�{@��#@��@�O�@�/@��@��@�Ĝ@��@��@��@���@�r�@�Z@�1'@��@�dZ@�C�@�"�@�@��@���@�~�@�-@�J@�@�@��@��#@��^@�X@��`@��9@�r�@�  @��
@��P@�t�@�"�@��H@��H@�^5@���@���@�Z@� �@���@��F@��F@��@�dZ@�"�@�@��@��@���@�ȴ@��!@��+@�=q@��@���@�G�@�/@�Ĝ@��D@�bN@�ƨ@��@�l�@�
=@��y@��!@��@��@�?}@�7L@��@�%@��`@��u@�j@�j@�1'@���@��@��y@��@���@�ȴ@���@���@�v�@�v�@�v�@�v�@�v�@�n�@�V@�-@�J@��#@�x�@�7L@��@���@��@�1'@��@��;@���@�|�@�t�@�;d@��@��@���@��R@�ȴ@��\@�V@�@��#@���@�p�@�G�@�V@���@���@��9@�bN@�(�@�1@�  @��m@���@��@���@��P@�|�@�C�@�
=@�@��@��!@���@���@���@��y@���@�ȴ@���@�n�@�M�@��@���@�p�@�?}@���@�Z@� �@���@���@�t�@�C�@�+@��@��H@��R@���@�~�@���@���@��@�V@�{@���@��-@�p�@�?}@�%@��@��@�Ĝ@�  @�P@\)@+@�@~�@~5?@}�@}�@}�@}�@|�@|�j@|I�@|(�@{��@{�
@{��@z�H@z~�@z~�@z�\@z�\@z�@y�7@x��@xr�@w��@w�@v�@vȴ@v�+@v5?@u�@t��@s�@sC�@r�!@rn�@rJ@q7L@pQ�@o�;@ol�@o�@nȴ@n��@m�@l9X@l1@k�
@kƨ@kdZ@j�!@j^5@j=q@jJ@i��@i��@ix�@h��@h�9@h��@hr�@hA�@h1'@h  @g�;@g��@g��@g��@gK�@f�@fV@eO�@d�j@d1@cƨ@c��@c��@ct�@cS�@cC�@c"�@b�@b��@bM�@a�@a��@a�^@a��@ahs@a7L@`��@`Ĝ@`�@`A�@`  @`  @_�;@_�@_l�@^ȴ@^V@^E�@]@]�@]V@\�@\�@[�
@[�@Z�H@Z~�@Z=q@Y��@X��@X��@X�@X�@Xr�@XbN@XA�@X1'@X �@W��@W�@Wl�@W\)@W;d@W+@V�y@Vȴ@V�+@VV@U�-@U�@T�@T�D@T�D@T�D@Tj@T1@S�
@S"�@R��@RJ@Qhs@Q�@Q%@Q�@P��@P�`@P��@PĜ@P�9@P�9@P�u@Pr�@O�@O+@N�R@N��@N5?@M�@MV@L�@Lz�@LZ@L9X@L1@K�m@K��@KdZ@J�@Jn�@J-@I��@IX@I&�@I�@H��@H �@Hb@G�@G��@GK�@G
=@Fȴ@Fv�@FE�@E�@E��@E@Ep�@D�j@Dz�@D9X@D�@C�m@C�@CS�@B�@B��@B~�@B=q@BJ@A�@A��@AX@A%@@�9@@�@@bN@@1'@@  @?�@?�P@?K�@>{@=��@=�h@=O�@<��@<�j@<�@<�D@<(�@:=q@9�@8�`@8��@8��@8��@8Ĝ@8��@8��@8�@7�@7�P@7+@7
=@6��@6V@5�@5�-@5�h@5�@5O�@4��@4�@4�/@4�j@4�D@4Z@49X@4(�@4�@3��@3dZ@1��@1�^@1��@1hs@17L@17L@1�@0��@0�@0b@/�@/K�@/+@.�@.ȴ@.ȴ@.ȴ@.��@.V@.5?@-�T@-�h@-O�@-V@,�@,1@+�
@+ƨ@+ƨ@+t�@*�@*�!@*~�@*�@)�#@)�^@)hs@(��@(r�@( �@(b@(  @'�@'
=@&��@&v�@&ff@&V@&E�@&E�@&$�@%@%p�@%`B@%�@$�D@$z�@$I�@#��@#��@#S�@#33@"�H@"�!@"�!@"��@"M�@ ��@ Q�@ 1'@  �@   @�@l�@��@p�@�D@j@I�@�@ƨ@��@S�@33@��@��@�!@�!@n�@-@-@-@J@��@hs@X@G�@7L@&�@&�@��@�9@Q�@A�@  @��@��@\)@
=@�y@�R@�R@��@�+@V@{@@O�@�j@�@��@�m@��@dZ@C�@"�@@�@��@�\@n�@M�@��@��@x�@hs@hs@X@��@�9@�9@��@�u@A�@�@��@��@�w@�w@�@�@��@�P@K�@
=@�R@��@ff@V@5?@$�@{@�@��@@�-@�h@/@��@�j@��@�D@z�@z�@j@Z@(�@ƨ@ƨ@��@��@��@dZ@o@
~�@	��@	�@��@b@�@��@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�9XA�9XA�7LA�5?A�;dA�9XA�9XA�;dA�;dA�?}A�A�A�?}A�;dA�9XA��A�-AυA��mA���AȺ^AǛ�A�33A���A�E�A�A�A�A�n�A�{A���A�Q�A��FA�1A���A��yA�ZA��`A���A�t�A���A�O�A�n�A��A��7A���A�^5A�JA�`BA��PA��
A��A�A���A�G�A���A��jA�5?A��!A�oA�z�A���A��`A�XA�"�A��\A���A�"�A�C�A�K�A��uA��yA��\A�=qA�dZA�Q�A�/A��A�A�A��9A�p�A��PA�E�A�A�A�XA�"�A���A���A�O�A��Ap�A~$�A|�!Az5?Ay7LAxbAwt�Av$�AtQ�Ar�AoS�Amt�AkƨAg"�Adr�Act�AbȴA`�jA^1'AY�
AW��AV(�AS�ARM�AP~�AM�^AK�mAJ�!AI��AH �AG�^AG��AG`BAE��AC�TAC`BAB$�A?�;A>$�A<�yA;��A9��A8(�A6r�A5VA3\)A1O�A.��A-�#A,ffA++A(�A&ȴA%A$�yA$�uA#�#A"��A"��A"v�A!|�A Q�AS�AȴA�AO�A��A^5A��A33A�yA��A=qA�A�A��A�!AA�A{AA��A�`AbA��A33A%A��A�A��A33AM�A��AA	p�AĜA��AI�A��Ap�A33AVA��A1A�FA�PA"�AhsAJA33A�A z�@�t�@�ȴ@���@���@��@���@�`B@�/@��@�Z@�+@��@�%@�Z@��
@�@�C�@�-@�b@�V@�x�@�7@웦@��@��#@�^@�x�@�33@��`@�D@��;@�C�@�~�@�-@�@�/@�1@�l�@�M�@ݑh@�X@���@ڏ\@��m@���@�+@ҟ�@�M�@�x�@мj@� �@ΰ!@�J@��T@͙�@��@̓u@�+@ʏ\@ɲ-@ȓu@���@Ǿw@�t�@���@Ƨ�@�v�@�J@���@ź^@�O�@��@�Ĝ@�r�@�  @Õ�@���@�-@��^@���@���@�p�@��@�r�@��@�\)@�ff@�@���@��/@��@��u@��@�l�@��!@�V@���@��-@��u@�33@�=q@�`B@�1'@��
@���@��\@�M�@�@���@���@�x�@�`B@�7L@���@��u@�bN@�Q�@�(�@���@�\)@�{@��^@���@�?}@�&�@��j@��@�ȴ@��@��@�@�o@�33@�C�@��@��!@�V@�J@��@��@�A�@���@���@���@�t�@�C�@���@�v�@�=q@���@��h@��h@��7@�G�@��`@���@�z�@�A�@�1@��;@�\)@��@�{@��#@��@�O�@�/@��@��@�Ĝ@��@��@��@���@�r�@�Z@�1'@��@�dZ@�C�@�"�@�@��@���@�~�@�-@�J@�@�@��@��#@��^@�X@��`@��9@�r�@�  @��
@��P@�t�@�"�@��H@��H@�^5@���@���@�Z@� �@���@��F@��F@��@�dZ@�"�@�@��@��@���@�ȴ@��!@��+@�=q@��@���@�G�@�/@�Ĝ@��D@�bN@�ƨ@��@�l�@�
=@��y@��!@��@��@�?}@�7L@��@�%@��`@��u@�j@�j@�1'@���@��@��y@��@���@�ȴ@���@���@�v�@�v�@�v�@�v�@�v�@�n�@�V@�-@�J@��#@�x�@�7L@��@���@��@�1'@��@��;@���@�|�@�t�@�;d@��@��@���@��R@�ȴ@��\@�V@�@��#@���@�p�@�G�@�V@���@���@��9@�bN@�(�@�1@�  @��m@���@��@���@��P@�|�@�C�@�
=@�@��@��!@���@���@���@��y@���@�ȴ@���@�n�@�M�@��@���@�p�@�?}@���@�Z@� �@���@���@�t�@�C�@�+@��@��H@��R@���@�~�@���@���@��@�V@�{@���@��-@�p�@�?}@�%@��@��@�Ĝ@�  @�P@\)@+@�@~�@~5?@}�@}�@}�@}�@|�@|�j@|I�@|(�@{��@{�
@{��@z�H@z~�@z~�@z�\@z�\@z�@y�7@x��@xr�@w��@w�@v�@vȴ@v�+@v5?@u�@t��@s�@sC�@r�!@rn�@rJ@q7L@pQ�@o�;@ol�@o�@nȴ@n��@m�@l9X@l1@k�
@kƨ@kdZ@j�!@j^5@j=q@jJ@i��@i��@ix�@h��@h�9@h��@hr�@hA�@h1'@h  @g�;@g��@g��@g��@gK�@f�@fV@eO�@d�j@d1@cƨ@c��@c��@ct�@cS�@cC�@c"�@b�@b��@bM�@a�@a��@a�^@a��@ahs@a7L@`��@`Ĝ@`�@`A�@`  @`  @_�;@_�@_l�@^ȴ@^V@^E�@]@]�@]V@\�@\�@[�
@[�@Z�H@Z~�@Z=q@Y��@X��@X��@X�@X�@Xr�@XbN@XA�@X1'@X �@W��@W�@Wl�@W\)@W;d@W+@V�y@Vȴ@V�+@VV@U�-@U�@T�@T�D@T�D@T�D@Tj@T1@S�
@S"�@R��@RJ@Qhs@Q�@Q%@Q�@P��@P�`@P��@PĜ@P�9@P�9@P�u@Pr�@O�@O+@N�R@N��@N5?@M�@MV@L�@Lz�@LZ@L9X@L1@K�m@K��@KdZ@J�@Jn�@J-@I��@IX@I&�@I�@H��@H �@Hb@G�@G��@GK�@G
=@Fȴ@Fv�@FE�@E�@E��@E@Ep�@D�j@Dz�@D9X@D�@C�m@C�@CS�@B�@B��@B~�@B=q@BJ@A�@A��@AX@A%@@�9@@�@@bN@@1'@@  @?�@?�P@?K�@>{@=��@=�h@=O�@<��@<�j@<�@<�D@<(�@:=q@9�@8�`@8��@8��@8��@8Ĝ@8��@8��@8�@7�@7�P@7+@7
=@6��@6V@5�@5�-@5�h@5�@5O�@4��@4�@4�/@4�j@4�D@4Z@49X@4(�@4�@3��@3dZ@1��@1�^@1��@1hs@17L@17L@1�@0��@0�@0b@/�@/K�@/+@.�@.ȴ@.ȴ@.ȴ@.��@.V@.5?@-�T@-�h@-O�@-V@,�@,1@+�
@+ƨ@+ƨ@+t�@*�@*�!@*~�@*�@)�#@)�^@)hs@(��@(r�@( �@(b@(  @'�@'
=@&��@&v�@&ff@&V@&E�@&E�@&$�@%@%p�@%`B@%�@$�D@$z�@$I�@#��@#��@#S�@#33@"�H@"�!@"�!@"��@"M�@ ��@ Q�@ 1'@  �@   @�@l�@��@p�@�D@j@I�@�@ƨ@��@S�@33@��@��@�!@�!@n�@-@-@-@J@��@hs@X@G�@7L@&�@&�@��@�9@Q�@A�@  @��@��@\)@
=@�y@�R@�R@��@�+@V@{@@O�@�j@�@��@�m@��@dZ@C�@"�@@�@��@�\@n�@M�@��@��@x�@hs@hs@X@��@�9@�9@��@�u@A�@�@��@��@�w@�w@�@�@��@�P@K�@
=@�R@��@ff@V@5?@$�@{@�@��@@�-@�h@/@��@�j@��@�D@z�@z�@j@Z@(�@ƨ@ƨ@��@��@��@dZ@o@
~�@	��@	�@��@b@�@��@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B{�Bs�BgmBl�B}�B��BǮB�B�TB��B1B
=B�B�B"�B)�B0!B:^BN�Bz�B�B�jB�B�B��B��B��B�B��B�1B�B~�B�BcTBp�B��B�?BŢBǮBǮB��B�jB�RB�'B�\Bz�BVB33B	7B�B�5BƨBhsB�B
�fB
ĜB
��B
�%B
z�B
hsB
iyB
s�B
��B
�B
��B
�1B
m�B
P�B
P�B
O�B
M�B
I�B
G�B
C�B
49B
)�B
�B
�B
VB

=B
B	��B	�B	�mB	�5B	��B	�XB	��B	��B	��B	�JB	~�B	aHB	W
B	K�B	A�B	8RB	/B	%�B	�B	{B	bB	
=B	1B	+B	B	B��B��B�B�mB�HB�#B�B��B��BƨB��B�wB�dB�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�uB�uB�uB�hB�bB�oB�uB�oB�oB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�?B�FB�XB�qB�wB�wB�}BĜBƨBǮBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�5B�BB�HB�HB�BB�BB�NB�ZB�sB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	  B��B��B��B��B��B��B��B��B��B��B��B	B	+B		7B		7B	DB	PB	bB	�B	�B	�B	�B	!�B	"�B	#�B	&�B	'�B	'�B	)�B	+B	+B	-B	.B	0!B	1'B	33B	6FB	9XB	=qB	@�B	@�B	@�B	A�B	C�B	G�B	H�B	M�B	R�B	VB	ZB	[#B	\)B	]/B	`BB	cTB	ffB	gmB	iyB	jB	l�B	p�B	t�B	y�B	}�B	�B	�B	�+B	�1B	�DB	�JB	�JB	�VB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�-B	�9B	�?B	�RB	�XB	�^B	�dB	�^B	�^B	�jB	�^B	�qB	��B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�NB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
JB
PB
PB
VB
VB
bB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
+B
+B
,B
,B
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
/B
/B
/B
/B
0!B
0!B
1'B
0!B
1'B
1'B
33B
49B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
7LB
6FB
6FB
6FB
6FB
5?B
5?B
49B
49B
5?B
5?B
6FB
6FB
6FB
5?B
6FB
8RB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
<jB
<jB
=qB
=qB
>wB
=qB
=qB
<jB
<jB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
A�B
A�B
A�B
A�B
B�B
C�B
B�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
J�B
K�B
K�B
L�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
bNB
cTB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
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
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
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
x�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�%B
�%B
�+B
�+B
�1B
�1B
�+B
�1B
�7B
�7B
�7B
�=B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�hB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
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
��B
��B
��B
��B
��B
�B
�B
�B
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B{�Bs�BgmBl�B}�B��BǮB�B�TB��B1B
=B�B�B"�B)�B0!B:^BN�Bz�B�B�jB�B�B��B��B��B�B��B�1B�B~�B�BcTBp�B��B�?BŢBǮBǮB��B�jB�RB�'B�\Bz�BVB33B	7B�B�5BƨBhsB�B
�fB
ĜB
��B
�%B
z�B
hsB
iyB
s�B
��B
�B
��B
�1B
m�B
P�B
P�B
O�B
M�B
I�B
G�B
C�B
49B
)�B
�B
�B
VB

=B
B	��B	�B	�mB	�5B	��B	�XB	��B	��B	��B	�JB	~�B	aHB	W
B	K�B	A�B	8RB	/B	%�B	�B	{B	bB	
=B	1B	+B	B	B��B��B�B�mB�HB�#B�B��B��BƨB��B�wB�dB�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�uB�uB�uB�hB�bB�oB�uB�oB�oB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�?B�FB�XB�qB�wB�wB�}BĜBƨBǮBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�5B�BB�HB�HB�BB�BB�NB�ZB�sB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	  B��B��B��B��B��B��B��B��B��B��B��B	B	+B		7B		7B	DB	PB	bB	�B	�B	�B	�B	!�B	"�B	#�B	&�B	'�B	'�B	)�B	+B	+B	-B	.B	0!B	1'B	33B	6FB	9XB	=qB	@�B	@�B	@�B	A�B	C�B	G�B	H�B	M�B	R�B	VB	ZB	[#B	\)B	]/B	`BB	cTB	ffB	gmB	iyB	jB	l�B	p�B	t�B	y�B	}�B	�B	�B	�+B	�1B	�DB	�JB	�JB	�VB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�-B	�9B	�?B	�RB	�XB	�^B	�dB	�^B	�^B	�jB	�^B	�qB	��B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�NB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
JB
PB
PB
VB
VB
bB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
+B
+B
,B
,B
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
/B
/B
/B
/B
0!B
0!B
1'B
0!B
1'B
1'B
33B
49B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
7LB
6FB
6FB
6FB
6FB
5?B
5?B
49B
49B
5?B
5?B
6FB
6FB
6FB
5?B
6FB
8RB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
<jB
<jB
=qB
=qB
>wB
=qB
=qB
<jB
<jB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
A�B
A�B
A�B
A�B
B�B
C�B
B�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
J�B
K�B
K�B
L�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
bNB
cTB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
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
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
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
x�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�%B
�%B
�+B
�+B
�1B
�1B
�+B
�1B
�7B
�7B
�7B
�=B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�hB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
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
��B
��B
��B
��B
��B
�B
�B
�B
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230710064311  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230709214324  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230709214324  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230709214325                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230709214326  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230709214326  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230709215711                      G�O�G�O�G�O�                