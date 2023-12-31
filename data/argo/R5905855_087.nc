CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:25:58Z creation;2022-06-04T19:25:58Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192558  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               WA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ـ뵪J1   @ـZt�@*�-�d7KƧ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@y��@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  CL�C�fC  C  C   C"  C$�C&  C'�fC)�fC,  C.  C0  C2  C4  C6  C8  C:�C<�C>� C?�fCA�fCD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@.{@tz�@�p�@�p�A�RA>�RA`Q�A~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B�=pB��
B��
B��
B��
B��
B��
B�
=B�p�B��
B��
B��
B��
B��
Bϣ�Bӣ�B��
B��
B��
B��
B��
B��
B�=pB��
B��
B��
B��
C��C�C�C�C	�C�C�C�C�C�C�C8RC��C�C�C�C!�C$C%�C'��C)��C+�C-�C/�C1�C3�C5�C7�C:C<C>k�C?��CA��CC�CE�CG�CI��CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�CfCg�Ci�Ck��Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$�GD%GD%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�m]A�w�A�zxA�tTA�v`A�t�A�d�A�d&A�h
A�[�A��]A��2A�бA�ӏA�ӏA���A��)A��EA�ƨA�Aо�Aк*AвaAЦ�AЖ�A�y�A�8�A��TAϭ�A�I�A�3hA�A��A�A��A��#AΡ�A��A��A��A�%FA�:�A�dZA�ޞA�A�ŢA¾BA�j�A��A��nA��aA�1�A�MA�Z�A���A��A�A�A�IRA��A�49A��A��1A��A���A��A�u%A��MA���A�M6A�3hA���A�ɺA�%A�=A�7A���A�<�A���A��oA�9�A��6A��tA�_;A��KA�9�A�ںA���A�>�A���A��A���A��rA�A���A~��Ax4nAt!�Alt�Af�}A`��AXg8ATk�AQ�XANO�AL��AK�AJ�HAH��AG6�AE6zAC7LAA�VA>j�A;�gA;J�A:4�A9�6A8�DA8�A6�*A5Z�A4Z�A3��A3��A3}VA3�A2�HA2��A2m]A1��A1��A1�A1��A0M�A/�*A/�bA/��A/�3A/��A/��A.��A.y>A.�{A.�vA-��A,\�A*�PA)e,A(��A'A&_pA%y�A$|�A"I�A jA �A!�A ��A bNA #�A��A�AߤA��A�mA�A�EA�AjA\�A5�A�&A�.A+kA�'A/�A�AݘAںA�qA_pA8�A�A��AcA�A�"A$tA�eA|�A� A�A:�A�]AOvA��Au%AsAc�AV�A4A$AA��A�NA�fAVA��A�A��A1'A-�A�`A�'Aw�A�Ao�AC�A��AL0A�[ARTA��AXyA
�A
�~A
:*A	�.A	H�Af�A�AaA��AaA�A0UA��A��AdZA33A �A��A��AJ�A��A��Ag8AFtA@A��A خ@���@���@���@���@�tT@�;�@���@�j�@��M@��@���@��I@��@�g�@�S�@�Q�@�<6@�+k@��@��O@��@��g@�O@��"@���@���@��T@�<�@�*0@�#:@��@�M@��@� \@�7@��@�4@�c�@��@��@薼@�5?@���@曦@��@�M@�8�@��@��@�YK@���@�k@㋬@��@�:*@ᦵ@�@�e@��@�%F@�oi@���@ݹ�@�l�@�p;@ۜ�@�]�@�j@�l�@ؙ1@���@�(@���@֜x@�'R@�iD@���@�i�@ӥ�@�(�@���@�	@ѽ�@фM@��@иR@�@�@�|�@��@�5?@Ͳ-@�`B@�L�@�=�@���@�c�@�O@��@�IR@�S@���@�Ov@� �@ɗ$@�q@��@�Vm@Ə\@���@Ů�@�*0@��H@Ĩ�@�`�@�"h@��o@ü�@�j�@��@��@���@�v`@�1�@���@�E�@��@��	@�R�@���@��6@�Ft@�b@��0@�J#@��R@���@�R�@��@���@���@�/@���@��@��@��,@��R@�~�@�H�@��m@���@���@�s�@�Z�@�2a@��v@��o@���@�n/@�G�@��@��@���@�s�@�	@���@�a@���@�h�@��@���@���@�l�@�2a@���@��U@��+@��o@�6z@��?@�?�@��@���@���@�@@��v@�m�@���@��	@��H@�_p@��@�l�@��@��e@�l�@���@�[W@�!-@��'@�1@�n/@��2@��e@���@�l"@�=q@��@��g@��~@���@�9X@��3@�F@��@���@��b@�|�@�g8@�M�@�1�@��@��=@��@���@�p;@��N@��@��1@�/�@�(�@�%�@��@� �@��^@���@��!@�[�@�!@���@�e�@�-w@��]@�w�@�1�@� �@��@�
�@��D@��*@�_p@�@@��@�ff@��@��w@�H�@�o@��[@���@�h�@�<�@��A@��V@�)_@���@�ff@�-@���@���@���@�c @� �@���@�G�@��@��+@�S�@� �@��>@���@��/@�I�@��@��+@��h@�8@��@��$@��+@�.�@��.@��Q@��k@�#�@��@�Ta@��)@���@��t@��@��@�s@�e�@�:�@��?@�E�@�#:@��@���@�C@���@��F@�V�@�&�@��T@���@�s@�4@��K@��@�YK@�2�@��@�@~�F@}��@|�E@|��@|m�@{�+@{�:@{A�@z�@z� @zR�@y�@y4@x�/@xN�@w��@wdZ@w"�@v��@v#:@u��@u:�@t�[@s�g@s
=@r�@q�@p�z@p�@o�k@nȴ@nh
@n@mJ�@l�5@l��@l�D@l`�@k�r@k|�@k+@j�@j6�@i��@iN<@hN�@h'R@g˒@gS�@f�"@f�'@f�b@e��@d�Y@c�A@c�g@c��@cC@b��@b�b@aԕ@azx@a@`e�@`  @_��@_O@^��@^��@^{�@^R�@^ �@]��@]:�@\�`@\V�@\@[��@[�@@[�{@[�@[y�@[S�@Z͟@ZB[@Z	@Y�j@Y�n@Yk�@X��@W��@W'�@WS@V��@V�h@V��@V��@V�\@V��@Vz@Vv�@Vff@V@�@V	@U�M@U+�@T�5@T��@T��@T�O@Tu�@T$@S�{@S�@R�A@Ru@Q�7@Q2a@P�/@P��@P2�@O�m@O��@OZ�@N�@N#:@M�C@Mu�@M%F@LFt@Ky�@KC@J�c@Jȴ@J~�@JO@I�C@IVm@H��@Hl"@G��@GO@F�,@F��@F�+@F	@D�)@C�@C�*@CiD@CY@B��@Bv�@Bq�@B-@Aϫ@Ao @@�U@@m�@@b@?� @?~�@?,�@>�"@>� @>6�@>($@=�@=�@=`B@<��@<��@<7�@<�@;�@;�A@;�}@;��@;��@;e�@;E9@:�"@:C�@9c@8�@8�@89X@7�Q@7;d@6�@6��@6��@6q�@6.�@5�T@5�@5w2@5e,@5a�@55�@4��@4�@3�r@3��@3s@3@O@2��@2�@2�1@2z@2Ta@2=q@20U@1�@1�@0��@0z�@0j@0g8@0C-@/�@/�@.��@.��@.~�@.GE@.-@-��@-�@-`B@,�f@,�[@,Ɇ@,��@,~@+��@+��@+��@+@O@*v�@*3�@)�@)��@)%F@(�v@(�D@(<�@'�}@'=@&�'@&1�@&�@%�D@%��@%m]@%8�@%7L@%5�@%#�@%�@%q@$Ĝ@$6@#�@#�a@#��@#�4@#iD@#@"�A@"M�@"�@!�^@!�@!k�@!S&@!?}@!�@ ��@ ��@ c�@ 2�@��@�&@�q@a@$t@��@�R@�A@J�@-@�@��@p�@=�@��@�e@u�@tT@_@[�@M@7�@,=@7@�@�}@�P@�@��@R�@$�@�D@�@�d@�@�=@c@|@X@Dg@@@�)@|�@`�@U2@S�@N�@H@,=@@��@�$@{J@U�@)_@�@�]@�+@p;@\�@;�@ �@��@��@zx@T�@G�@7L@�@�9@�u@�o@_@%�@��@�F@��@�{@J#@�@�\@��@i�@GE@	@��@��@��@`B@8�@@@�@%@�@��@��@S�@K^@"h@�
@�6@�K@��@X�@��@��@ں@�}@��@�A@~�@Z�@J�@1�@�@�#@��@zx@Y�@X@O�@%F@�p@��@�D@<�@$@�+@��@�K@�@��@o�@,�@�@
�8@
�L@
��@
p;@
\�@
J�@
)�@	��@	�N@	��@	��@	f�@	5�@	%F@	�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�m]A�w�A�zxA�tTA�v`A�t�A�d�A�d&A�h
A�[�A��]A��2A�бA�ӏA�ӏA���A��)A��EA�ƨA�Aо�Aк*AвaAЦ�AЖ�A�y�A�8�A��TAϭ�A�I�A�3hA�A��A�A��A��#AΡ�A��A��A��A�%FA�:�A�dZA�ޞA�A�ŢA¾BA�j�A��A��nA��aA�1�A�MA�Z�A���A��A�A�A�IRA��A�49A��A��1A��A���A��A�u%A��MA���A�M6A�3hA���A�ɺA�%A�=A�7A���A�<�A���A��oA�9�A��6A��tA�_;A��KA�9�A�ںA���A�>�A���A��A���A��rA�A���A~��Ax4nAt!�Alt�Af�}A`��AXg8ATk�AQ�XANO�AL��AK�AJ�HAH��AG6�AE6zAC7LAA�VA>j�A;�gA;J�A:4�A9�6A8�DA8�A6�*A5Z�A4Z�A3��A3��A3}VA3�A2�HA2��A2m]A1��A1��A1�A1��A0M�A/�*A/�bA/��A/�3A/��A/��A.��A.y>A.�{A.�vA-��A,\�A*�PA)e,A(��A'A&_pA%y�A$|�A"I�A jA �A!�A ��A bNA #�A��A�AߤA��A�mA�A�EA�AjA\�A5�A�&A�.A+kA�'A/�A�AݘAںA�qA_pA8�A�A��AcA�A�"A$tA�eA|�A� A�A:�A�]AOvA��Au%AsAc�AV�A4A$AA��A�NA�fAVA��A�A��A1'A-�A�`A�'Aw�A�Ao�AC�A��AL0A�[ARTA��AXyA
�A
�~A
:*A	�.A	H�Af�A�AaA��AaA�A0UA��A��AdZA33A �A��A��AJ�A��A��Ag8AFtA@A��A خ@���@���@���@���@�tT@�;�@���@�j�@��M@��@���@��I@��@�g�@�S�@�Q�@�<6@�+k@��@��O@��@��g@�O@��"@���@���@��T@�<�@�*0@�#:@��@�M@��@� \@�7@��@�4@�c�@��@��@薼@�5?@���@曦@��@�M@�8�@��@��@�YK@���@�k@㋬@��@�:*@ᦵ@�@�e@��@�%F@�oi@���@ݹ�@�l�@�p;@ۜ�@�]�@�j@�l�@ؙ1@���@�(@���@֜x@�'R@�iD@���@�i�@ӥ�@�(�@���@�	@ѽ�@фM@��@иR@�@�@�|�@��@�5?@Ͳ-@�`B@�L�@�=�@���@�c�@�O@��@�IR@�S@���@�Ov@� �@ɗ$@�q@��@�Vm@Ə\@���@Ů�@�*0@��H@Ĩ�@�`�@�"h@��o@ü�@�j�@��@��@���@�v`@�1�@���@�E�@��@��	@�R�@���@��6@�Ft@�b@��0@�J#@��R@���@�R�@��@���@���@�/@���@��@��@��,@��R@�~�@�H�@��m@���@���@�s�@�Z�@�2a@��v@��o@���@�n/@�G�@��@��@���@�s�@�	@���@�a@���@�h�@��@���@���@�l�@�2a@���@��U@��+@��o@�6z@��?@�?�@��@���@���@�@@��v@�m�@���@��	@��H@�_p@��@�l�@��@��e@�l�@���@�[W@�!-@��'@�1@�n/@��2@��e@���@�l"@�=q@��@��g@��~@���@�9X@��3@�F@��@���@��b@�|�@�g8@�M�@�1�@��@��=@��@���@�p;@��N@��@��1@�/�@�(�@�%�@��@� �@��^@���@��!@�[�@�!@���@�e�@�-w@��]@�w�@�1�@� �@��@�
�@��D@��*@�_p@�@@��@�ff@��@��w@�H�@�o@��[@���@�h�@�<�@��A@��V@�)_@���@�ff@�-@���@���@���@�c @� �@���@�G�@��@��+@�S�@� �@��>@���@��/@�I�@��@��+@��h@�8@��@��$@��+@�.�@��.@��Q@��k@�#�@��@�Ta@��)@���@��t@��@��@�s@�e�@�:�@��?@�E�@�#:@��@���@�C@���@��F@�V�@�&�@��T@���@�s@�4@��K@��@�YK@�2�@��@�@~�F@}��@|�E@|��@|m�@{�+@{�:@{A�@z�@z� @zR�@y�@y4@x�/@xN�@w��@wdZ@w"�@v��@v#:@u��@u:�@t�[@s�g@s
=@r�@q�@p�z@p�@o�k@nȴ@nh
@n@mJ�@l�5@l��@l�D@l`�@k�r@k|�@k+@j�@j6�@i��@iN<@hN�@h'R@g˒@gS�@f�"@f�'@f�b@e��@d�Y@c�A@c�g@c��@cC@b��@b�b@aԕ@azx@a@`e�@`  @_��@_O@^��@^��@^{�@^R�@^ �@]��@]:�@\�`@\V�@\@[��@[�@@[�{@[�@[y�@[S�@Z͟@ZB[@Z	@Y�j@Y�n@Yk�@X��@W��@W'�@WS@V��@V�h@V��@V��@V�\@V��@Vz@Vv�@Vff@V@�@V	@U�M@U+�@T�5@T��@T��@T�O@Tu�@T$@S�{@S�@R�A@Ru@Q�7@Q2a@P�/@P��@P2�@O�m@O��@OZ�@N�@N#:@M�C@Mu�@M%F@LFt@Ky�@KC@J�c@Jȴ@J~�@JO@I�C@IVm@H��@Hl"@G��@GO@F�,@F��@F�+@F	@D�)@C�@C�*@CiD@CY@B��@Bv�@Bq�@B-@Aϫ@Ao @@�U@@m�@@b@?� @?~�@?,�@>�"@>� @>6�@>($@=�@=�@=`B@<��@<��@<7�@<�@;�@;�A@;�}@;��@;��@;e�@;E9@:�"@:C�@9c@8�@8�@89X@7�Q@7;d@6�@6��@6��@6q�@6.�@5�T@5�@5w2@5e,@5a�@55�@4��@4�@3�r@3��@3s@3@O@2��@2�@2�1@2z@2Ta@2=q@20U@1�@1�@0��@0z�@0j@0g8@0C-@/�@/�@.��@.��@.~�@.GE@.-@-��@-�@-`B@,�f@,�[@,Ɇ@,��@,~@+��@+��@+��@+@O@*v�@*3�@)�@)��@)%F@(�v@(�D@(<�@'�}@'=@&�'@&1�@&�@%�D@%��@%m]@%8�@%7L@%5�@%#�@%�@%q@$Ĝ@$6@#�@#�a@#��@#�4@#iD@#@"�A@"M�@"�@!�^@!�@!k�@!S&@!?}@!�@ ��@ ��@ c�@ 2�@��@�&@�q@a@$t@��@�R@�A@J�@-@�@��@p�@=�@��@�e@u�@tT@_@[�@M@7�@,=@7@�@�}@�P@�@��@R�@$�@�D@�@�d@�@�=@c@|@X@Dg@@@�)@|�@`�@U2@S�@N�@H@,=@@��@�$@{J@U�@)_@�@�]@�+@p;@\�@;�@ �@��@��@zx@T�@G�@7L@�@�9@�u@�o@_@%�@��@�F@��@�{@J#@�@�\@��@i�@GE@	@��@��@��@`B@8�@@@�@%@�@��@��@S�@K^@"h@�
@�6@�K@��@X�@��@��@ں@�}@��@�A@~�@Z�@J�@1�@�@�#@��@zx@Y�@X@O�@%F@�p@��@�D@<�@$@�+@��@�K@�@��@o�@,�@�@
�8@
�L@
��@
p;@
\�@
J�@
)�@	��@	�N@	��@	��@	f�@	5�@	%F@	�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
B
�B
oB
 �B
 B
B	��B	�cB	�.B	�BB	��B	�zB	�zB	�2B	�B	�RB	��B	��B	��B	��B	��B	��B	��B	�TB	�-B	��B	�zB	ݘB	�sB	��B	�)B	�#B	��B	ɺB	�RB	�KB	ŢB	�B	�sB
AUB
W�B
u?B
��B
��B
��B
��B
�.B0B@B �B'B)B7LBGEBL~BbNBy�B�EB��B�)B�(B�B��B�GB�TB��B�wB�9B�JB�^B͹BοB��B�BԕB�VB��B�B�B�mB�"B�B�1By�BC�B,"BsB �B
��B
�WB
�{B
��B
j0B
G�B
�B	�B	�B	��B	zB	S�B	&B	HB	B��B�B�iB��B�B�ZB�B��B�tB�wB�B	B	�B	#�B	)DB	0oB	5?B	E�B	NpB	T�B	XyB	\�B	fLB	l�B	sB	{�B	�aB	�TB	�B	��B	��B	��B	��B	��B	�B	�hB	��B	��B
�B
_B
-]B
./B
'RB
$&B
!|B
+�B
#TB
-CB
/B
*B
�B
�B
&�B
3�B
2|B
0B
2GB
4nB
0�B
,�B
-B
7�B
J�B
SuB
RTB
RB
R:B
RoB
T�B
VmB
W?B
X�B
Y�B
ZQB
ZB
Y�B
Z�B
[�B
[�B
ZB
YKB
X�B
ZQB
Z�B
[#B
Y�B
YB
W�B
T�B
Q�B
O�B
M�B
LJB
MB
M�B
M�B
M�B
M�B
MPB
M6B
L�B
LB
JrB
J	B
GzB
E9B
C�B
CaB
BuB
A�B
A;B
AB
@B
?B
>BB
=�B
="B
=<B
<�B
<6B
;B
;B
;B
9�B
9XB
8�B
7�B
6�B
6�B
4TB
2�B
0�B
.�B
-�B
/OB
/�B
.}B
.�B
,�B
+�B
*eB
)_B
)B
(�B
'�B
'8B
&2B
#�B
 \B
�B
/B
�B
�B
CB
�B
�B
�B
1B
EB

B
�B
�B
�B
MB
�B
FB
�B
�B
�B
4B
}B
}B
�B
�B
TB
4B
�B
�B
�B
�B
pB
"B
�B
~B
�B
�B

�B

=B
	�B
	�B
	7B
fB
�B
EB
�B
�B
�B
B
�B
�B
EB
+B
�B
�B
B
�B
�B
�B
aB
�B
uB
AB
�B
UB
 OB
 B	��B	��B	�BB	�(B	��B	��B	��B	��B	�"B	�qB	��B	��B	��B	�(B	�(B	�BB	�BB	�(B	�(B	��B	�]B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�(B	��B	��B	��B	�VB	�qB	��B	��B	��B	�B	�BB	�(B	�BB	��B	��B	�qB	�BB	�BB	�BB	��B	�qB	��B	��B	��B	��B	��B	��B	��B	�wB	��B	��B
  B
  B
 B
 �B
B
B
 �B
UB
UB
 B
 B
oB
UB
;B
;B
;B
;B
oB
�B
AB
[B
�B
�B
[B
[B
�B
�B
�B
�B
�B
3B
B
MB
�B
�B
�B
%B
�B
zB
zB
+B
_B
�B
B
KB
�B
�B
	�B

	B

XB

�B

�B

�B

�B
xB
xB
�B
�B
B
�B
�B
BB
�B
�B
�B
�B
4B
NB
hB
hB
�B
�B
oB
TB
�B
�B
�B
�B
B
&B
�B
aB
{B
MB
MB
�B
�B
�B
�B
�B
�B
�B
�B
B
B
9B
�B
?B
�B
�B
�B
�B
�B
�B
�B
B
�B
KB
�B
�B
�B
kB
	B
�B
�B
)B
CB
)B
CB
�B
�B
~B
�B
�B
5B
�B
B
;B
pB
�B
�B
�B
 \B
 vB
!|B
!�B
"NB
"hB
#:B
#�B
#�B
#�B
$ZB
$�B
$�B
%,B
%`B
%�B
%�B
%�B
%�B
'B
(
B
(XB
(XB
(�B
)_B
)�B
)�B
*0B
*�B
*�B
*�B
+QB
+�B
,"B
,�B
,�B
,�B
,�B
-B
-CB
-�B
-�B
-�B
.�B
/OB
/iB
/�B
0B
0�B
1'B
1�B
1�B
1�B
2-B
2|B
2�B
2aB
2�B
2�B
2�B
2�B
3hB
4B
4B
4�B
5?B
5?B
5?B
5�B
5�B
5�B
5�B
5�B
5�B
6+B
6zB
6zB
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
8�B
9$B
9XB
9�B
;B
;dB
;�B
<B
<B
<�B
<�B
<�B
=qB
=VB
=<B
=VB
=B
>(B
?HB
@B
@4B
@�B
@�B
@�B
AoB
A�B
BB
BB
A�B
B�B
C{B
CGB
C-B
CaB
DB
DB
DMB
EB
E9B
E�B
FYB
FYB
FtB
F�B
G+B
GEB
G_B
G_B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
IB
IB
IB
IB
IB
I�B
I�B
I�B
I�B
I�B
I�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
L�B
L�B
MB
MB
MB
MB
M6B
M�B
NB
N�B
N�B
O(B
O(B
O�B
O�B
PB
P.B
PHB
P�B
Q4B
Q�B
Q�B
Q�B
RB
R�B
SuB
S�B
S�B
S�B
S�B
TB
TaB
T{B
T�B
UB
U�B
U�B
V9B
VmB
V9B
VmB
WsB
YB
Y�B
YB
YB
Y�B
Y�B
Y�B
ZB
ZkB
ZB
Z�B
[�B
\�B
\�B
]dB
]~B
]�B
]�B
^5B
^5B
^5B
^B
^B
^B
]�B
]�B
]�B
^B
^5B
^�B
_!B
_VB
_pB
_�B
_pB
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
aB
aHB
abB
a�B
a�B
a|B
a�B
a�B
a�B
bhB
b�B
b�B
b�B
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
fB
fB
ffB
fLB
f�B
f�B
gB
gRB
g�B
gmB
g�B
h
B
h$B
hXB
hXB
h�B
h�B
iB
i_B
i�B
jeB
j�B
j�B
kB
kQB
k�B
k�B
lWB
lWB
lWB
l�B
mB
mwB
mwB
m�B
m�B
nB
n}B
nIB
m�B
m�B
m�B
nB
nIB
m�B
ncB
o B
oB
oOB
o�B
pB
pUB
poB
poB
p�B
p�B
p�B
q[B
q�B
rB
rB
r-B
r-B
rGB
rGB
raB
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
t9B
tB
tB
t9B
tB
tB
tTB
tTB
tTB
t�B
t�B
uB
u?B
u�B
u�B
u�B
vB
vB
vB
v+B
vFB
v`B
vFB
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
xB
xB
xB
xlB
x�B
x�B
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
{B
{B
{0B
{JB
{�B
|B
|6B
|PB
|�B
|�B
}"B
}�B
}�B
}qB
}�B
}�B
~(B
~�B
~�B
~�B
~�B
.B
.B
HB
�B
�B
�4B
�4B
�OB
�OB
��B
�;B
�UB
��B
��B
��B
�oB
��B
��B
�'B
��B
��B
��B
�B
��B
�-B
�GB
�aB
�{B
��B
�{B
�aB
��B
��B
��B
�MB
��B
��B
��B
��B
��B
�B
��B
�mB
��B
��B
��B
�?B
�YB
��B
��B
��B
��B
�+B
�+B
�+B
��B
��B
��B
��B
�1B
�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
B
�B
oB
 �B
 B
B	��B	�cB	�.B	�BB	��B	�zB	�zB	�2B	�B	�RB	��B	��B	��B	��B	��B	��B	��B	�TB	�-B	��B	�zB	ݘB	�sB	��B	�)B	�#B	��B	ɺB	�RB	�KB	ŢB	�B	�sB
AUB
W�B
u?B
��B
��B
��B
��B
�.B0B@B �B'B)B7LBGEBL~BbNBy�B�EB��B�)B�(B�B��B�GB�TB��B�wB�9B�JB�^B͹BοB��B�BԕB�VB��B�B�B�mB�"B�B�1By�BC�B,"BsB �B
��B
�WB
�{B
��B
j0B
G�B
�B	�B	�B	��B	zB	S�B	&B	HB	B��B�B�iB��B�B�ZB�B��B�tB�wB�B	B	�B	#�B	)DB	0oB	5?B	E�B	NpB	T�B	XyB	\�B	fLB	l�B	sB	{�B	�aB	�TB	�B	��B	��B	��B	��B	��B	�B	�hB	��B	��B
�B
_B
-]B
./B
'RB
$&B
!|B
+�B
#TB
-CB
/B
*B
�B
�B
&�B
3�B
2|B
0B
2GB
4nB
0�B
,�B
-B
7�B
J�B
SuB
RTB
RB
R:B
RoB
T�B
VmB
W?B
X�B
Y�B
ZQB
ZB
Y�B
Z�B
[�B
[�B
ZB
YKB
X�B
ZQB
Z�B
[#B
Y�B
YB
W�B
T�B
Q�B
O�B
M�B
LJB
MB
M�B
M�B
M�B
M�B
MPB
M6B
L�B
LB
JrB
J	B
GzB
E9B
C�B
CaB
BuB
A�B
A;B
AB
@B
?B
>BB
=�B
="B
=<B
<�B
<6B
;B
;B
;B
9�B
9XB
8�B
7�B
6�B
6�B
4TB
2�B
0�B
.�B
-�B
/OB
/�B
.}B
.�B
,�B
+�B
*eB
)_B
)B
(�B
'�B
'8B
&2B
#�B
 \B
�B
/B
�B
�B
CB
�B
�B
�B
1B
EB

B
�B
�B
�B
MB
�B
FB
�B
�B
�B
4B
}B
}B
�B
�B
TB
4B
�B
�B
�B
�B
pB
"B
�B
~B
�B
�B

�B

=B
	�B
	�B
	7B
fB
�B
EB
�B
�B
�B
B
�B
�B
EB
+B
�B
�B
B
�B
�B
�B
aB
�B
uB
AB
�B
UB
 OB
 B	��B	��B	�BB	�(B	��B	��B	��B	��B	�"B	�qB	��B	��B	��B	�(B	�(B	�BB	�BB	�(B	�(B	��B	�]B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�(B	��B	��B	��B	�VB	�qB	��B	��B	��B	�B	�BB	�(B	�BB	��B	��B	�qB	�BB	�BB	�BB	��B	�qB	��B	��B	��B	��B	��B	��B	��B	�wB	��B	��B
  B
  B
 B
 �B
B
B
 �B
UB
UB
 B
 B
oB
UB
;B
;B
;B
;B
oB
�B
AB
[B
�B
�B
[B
[B
�B
�B
�B
�B
�B
3B
B
MB
�B
�B
�B
%B
�B
zB
zB
+B
_B
�B
B
KB
�B
�B
	�B

	B

XB

�B

�B

�B

�B
xB
xB
�B
�B
B
�B
�B
BB
�B
�B
�B
�B
4B
NB
hB
hB
�B
�B
oB
TB
�B
�B
�B
�B
B
&B
�B
aB
{B
MB
MB
�B
�B
�B
�B
�B
�B
�B
�B
B
B
9B
�B
?B
�B
�B
�B
�B
�B
�B
�B
B
�B
KB
�B
�B
�B
kB
	B
�B
�B
)B
CB
)B
CB
�B
�B
~B
�B
�B
5B
�B
B
;B
pB
�B
�B
�B
 \B
 vB
!|B
!�B
"NB
"hB
#:B
#�B
#�B
#�B
$ZB
$�B
$�B
%,B
%`B
%�B
%�B
%�B
%�B
'B
(
B
(XB
(XB
(�B
)_B
)�B
)�B
*0B
*�B
*�B
*�B
+QB
+�B
,"B
,�B
,�B
,�B
,�B
-B
-CB
-�B
-�B
-�B
.�B
/OB
/iB
/�B
0B
0�B
1'B
1�B
1�B
1�B
2-B
2|B
2�B
2aB
2�B
2�B
2�B
2�B
3hB
4B
4B
4�B
5?B
5?B
5?B
5�B
5�B
5�B
5�B
5�B
5�B
6+B
6zB
6zB
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
8�B
9$B
9XB
9�B
;B
;dB
;�B
<B
<B
<�B
<�B
<�B
=qB
=VB
=<B
=VB
=B
>(B
?HB
@B
@4B
@�B
@�B
@�B
AoB
A�B
BB
BB
A�B
B�B
C{B
CGB
C-B
CaB
DB
DB
DMB
EB
E9B
E�B
FYB
FYB
FtB
F�B
G+B
GEB
G_B
G_B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
IB
IB
IB
IB
IB
I�B
I�B
I�B
I�B
I�B
I�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
L�B
L�B
MB
MB
MB
MB
M6B
M�B
NB
N�B
N�B
O(B
O(B
O�B
O�B
PB
P.B
PHB
P�B
Q4B
Q�B
Q�B
Q�B
RB
R�B
SuB
S�B
S�B
S�B
S�B
TB
TaB
T{B
T�B
UB
U�B
U�B
V9B
VmB
V9B
VmB
WsB
YB
Y�B
YB
YB
Y�B
Y�B
Y�B
ZB
ZkB
ZB
Z�B
[�B
\�B
\�B
]dB
]~B
]�B
]�B
^5B
^5B
^5B
^B
^B
^B
]�B
]�B
]�B
^B
^5B
^�B
_!B
_VB
_pB
_�B
_pB
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
aB
aHB
abB
a�B
a�B
a|B
a�B
a�B
a�B
bhB
b�B
b�B
b�B
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
fB
fB
ffB
fLB
f�B
f�B
gB
gRB
g�B
gmB
g�B
h
B
h$B
hXB
hXB
h�B
h�B
iB
i_B
i�B
jeB
j�B
j�B
kB
kQB
k�B
k�B
lWB
lWB
lWB
l�B
mB
mwB
mwB
m�B
m�B
nB
n}B
nIB
m�B
m�B
m�B
nB
nIB
m�B
ncB
o B
oB
oOB
o�B
pB
pUB
poB
poB
p�B
p�B
p�B
q[B
q�B
rB
rB
r-B
r-B
rGB
rGB
raB
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
t9B
tB
tB
t9B
tB
tB
tTB
tTB
tTB
t�B
t�B
uB
u?B
u�B
u�B
u�B
vB
vB
vB
v+B
vFB
v`B
vFB
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
xB
xB
xB
xlB
x�B
x�B
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
{B
{B
{0B
{JB
{�B
|B
|6B
|PB
|�B
|�B
}"B
}�B
}�B
}qB
}�B
}�B
~(B
~�B
~�B
~�B
~�B
.B
.B
HB
�B
�B
�4B
�4B
�OB
�OB
��B
�;B
�UB
��B
��B
��B
�oB
��B
��B
�'B
��B
��B
��B
�B
��B
�-B
�GB
�aB
�{B
��B
�{B
�aB
��B
��B
��B
�MB
��B
��B
��B
��B
��B
�B
��B
�mB
��B
��B
��B
�?B
�YB
��B
��B
��B
��B
�+B
�+B
�+B
��B
��B
��B
��B
�1B
�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192558  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192558  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192558                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042606  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042606  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                