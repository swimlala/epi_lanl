CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:56:49Z creation;2022-06-04T17:56:50Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175649  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�O^��,`1   @�O_i�@0+C��%�b����o1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@y��@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh��Bp  By��B~��B���B�  B�  B�  B�  B���B�ffB���B���B���B���B���B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CA��CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{@tz�@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�(�A�\)A�\)A�\)A�\)A�\)B�BG�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bhz�Bo�ByG�B~z�B���B��
B��
B��
B��
B�p�B�=pB���B���B���B���B���B��
B��
B��
B��
B�
=B�=pBʣ�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�CC�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+��C-�C/�C1�C3�C5�C7�C9�C;�C=�C@CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�CvCxCy��C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DHGDHz�DH��DIz�DI��DJz�DJ��DK�GDK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��>D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:>D�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�R�A�RTA�T�A�]/A�]/A�c�A�e,A�rAˑ�Aˡ�A˓uAˍ�Aˉ�AˉAˊ=Aˎ�A˦�A��QA�4A��A�1�A�P�A�`A�jKA�rGA̩*A�0UA͆%A͎�A�ƨA��A���A�f2A���A�^�A�^A�gmA�eA��QA���A��zA�@�A�=A�P�A��fA�,�A��A�P�A��A�7�A��A�{A��qA�1�A��oA��A��'A��NA�[�A���A�YA��6A���A�9�A�e`A��A��MA� �A���A��A�iA���A�FtA� �A�a�A�+A�
�A�{JA��A�5A��A��A�!�A�]dA� 'A���A�`BA���A���A��tA���A���A�H�A��A���Ay�YAm�kAk�AiL0Af�KAc33A^OA\ �AYZAW2�AU��AT�[AR��APR�AM �AKN<AJ%�AGA AE��AC�&AB�AC&�ADX�AC��A>�A=;dA<=�A;f�A;�A;xlA;��A;�.A:��A8�KA61'A5�A4�"A1��A0��A.�A-
�A-;A,�>A)�mA(��A'!�A%zA#�gA!�A!�A �eA�A6zA�}A�:A��A��A7�A+kA�<A*0A�$AzA�A��A �A��AZA֡AYKA�mAD�A�&Ag�AqA4APHAA��A�ACA�DA��A0UAn/AR�A�HA�Aw�AA��A��A�.AFtA��A(�A
��A
OA	�A	�}A	aA	.IA	($A�8A�)A�eA&�A��Ac�A%FA��AqA(Ae�A/A�.A� A�#A@OA�bA4�AƨAV�A�A ��A ��A xlA M@�A�@��?@�+�@�w�@� �@���@�k�@�F�@���@�n/@�A�@��@���@���@���@�\�@���@�ȴ@�-�@�@��@��@�@�@�Q�@��@�7L@�u�@�Z@��@�a�@�B�@�33@���@�\�@��@�E�@�ݘ@��@�9X@��@��d@��|@�p;@�1@���@崢@�4�@���@���@�$�@�W?@��I@�4@ߓ�@���@�c @ފr@�	@��+@ݬq@��>@��@��@�}�@�+@��@���@��@�U2@ۥ@�}�@�8�@ڥz@��@���@ف�@��@ؾ�@؃@ח�@֏\@�خ@�e�@��@��@�͟@�z�@�2�@�a@Һ�@�`�@�~@��}@��@�GE@��)@ϧ�@ϵt@��'@�z�@΄�@�~(@�p;@�7@��N@�RT@��`@�n�@�c @˲-@ʱ�@�I�@���@ɖS@�A @ȵ�@�r�@�,=@ǝ�@�"�@ƌ@�g8@�0U@�
�@���@ńM@��)@���@�c�@�ی@�ff@��@���@�S�@�?}@���@�p;@�e@��m@���@��@���@���@�J#@��@��)@�g�@�@��6@�m�@�=q@��@�ϫ@���@�Mj@�&@���@���@�e@��P@�,�@��9@�C�@��@���@��k@�dZ@��@�M@�e,@� \@���@�\�@�'R@���@�C@�]d@�e@�G@��D@���@�=@���@�-�@�J@��@��
@���@�T�@��@���@��@��@�w�@�kQ@�  @���@�w2@�o�@�\�@�%F@���@���@���@��R@�y>@��@�O�@�q@���@�>B@���@��:@�U�@��@�]d@�7�@�
�@��S@�m]@�C�@��@��@��2@��x@�x@�ԕ@���@�'�@�C@��@��u@��>@���@�x�@�W?@�*0@��@��U@�l�@�G@�@���@�U�@�>�@�@���@�($@���@���@�o�@��@���@�YK@���@���@�^�@��9@�_@�$@���@�x@�'�@�$t@��4@�W�@���@��*@�k�@�ѷ@�A�@��@��:@�!-@�Ĝ@���@���@�kQ@��@��a@�X@�4�@�@@��@�u@��;@��w@��7@�0�@��@�tT@��@�خ@��S@��@��@��o@�?@��@��]@��@�� @���@��	@��9@�H�@�M@�� @�e,@���@��j@��$@�Z�@�kQ@���@�(�@��D@��'@�Y@���@�Z@�)�@��n@�o@�͟@��!@�I�@�@���@�(@��.@�A�@�C�@�H�@�=q@�)�@��@���@�e�@�>�@�
=@���@��$@�Ta@��@��@�v`@�	l@��@���@�}V@�1'@�b@�_@���@���@��d@��a@��C@��V@��	@���@�v`@�F�@��@��	@���@�1�@�@��@&@~�h@~Ov@}�j@}o @}?}@}&�@}@@|��@|��@|�D@|H@|�@{�a@{s@{U�@{4�@{+@z��@zH�@z+k@z�@z@y��@ye,@y5�@x��@xA�@w��@wy�@v��@v��@v��@vl�@v �@uj@t�[@t(�@s]�@r�r@rE�@r�@q�H@p�@o�]@o��@o9�@n�@n^5@nu@m��@l�`@lV�@l�@kt�@j�@j�\@i��@i&�@hN�@g�@g�P@g6z@f�R@e��@d�4@d�@c�@aB�@aY�@a��@a�^@a8�@`�D@`I�@_qv@_ i@^͟@^Ov@]rG@\�@\%�@[��@[x@Z��@ZYK@Y��@YT�@Y�@X�|@X�@Xѷ@X�@Xw�@XZ@X@W�@Wx@W1�@W(@V��@V�@V�}@V��@Vi�@V�@U��@U^�@U�@T�4@T��@T�@T-�@S�@Sb�@S_p@SZ�@SW?@S8@R�m@RYK@Q�X@Qw2@Q-w@P��@P��@P��@P��@P`�@O��@N҉@N��@NTa@N�@M��@M��@M&�@L�$@LH@K� @K�4@K�@J�,@J~�@J4@I��@Ihs@H��@H%�@G��@GH�@F��@F��@Fs�@Fd�@F�@E�@E/@D�|@D֡@D�z@DK^@D�@C��@C=@B�!@BC�@A�t@@�@@V�@?��@?��@>��@>	@=�@=��@<�@<��@<A�@;�@;v`@;E9@;&@:�,@:��@:��@:@�@9�#@9��@9^�@9IR@9u�@9Q�@9L�@9L�@9�@9;@8��@8_@7��@7|�@7E9@6�@6�@6h
@5��@5zx@5`B@5*0@4�@4�`@4��@4��@4�u@4tT@4Z@3�@3��@3]�@3�@2~�@2_�@2Ov@2@1�9@1�@1��@1p�@1k�@1[W@1(�@0��@0�Y@0e�@0_@09X@0 �@07@01@/�4@.�]@.��@.�}@.��@.{�@.xl@.q�@.Z�@-��@-��@-��@-Y�@-F@-*0@-�@-@,�f@,�E@,Ɇ@,��@,��@,�@+��@+C�@+�@*��@*L0@)��@)�9@)��@)(�@(��@(g8@(<�@(@'��@'\)@&��@&	@%��@%�S@%e,@%q@$֡@$I�@$�@$7@$M@$�@#��@#��@#J#@"�@"�}@"��@"+k@!��@!u�@!L�@!@ Ĝ@ �.@ m�@ (�@�
@��@j�@X�@�@�8@�@z@Ta@=q@&�@�)@�'@�@Q�@��@S�@-�@�;@ƨ@��@U�@.I@�@�2@�b@_�@_@��@\�@-w@�)@��@�z@��@]d@4n@��@�f@J#@�@�2@��@��@}V@q�@l�@E�@�@�D@�S@-w@��@��@/�@"h@�@�@1@j�@�"@�@��@q�@=q@��@��@|@f�@F@!�@;@�/@�$@y>@V�@H@*�@~@�@�
@�0@�[@��@��@��@��@�4@qv@;d@4�@.I@"�@�@�M@�@�L@��@L0@��@�@p�@Vm@?}@�v@�4@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�R�A�RTA�T�A�]/A�]/A�c�A�e,A�rAˑ�Aˡ�A˓uAˍ�Aˉ�AˉAˊ=Aˎ�A˦�A��QA�4A��A�1�A�P�A�`A�jKA�rGA̩*A�0UA͆%A͎�A�ƨA��A���A�f2A���A�^�A�^A�gmA�eA��QA���A��zA�@�A�=A�P�A��fA�,�A��A�P�A��A�7�A��A�{A��qA�1�A��oA��A��'A��NA�[�A���A�YA��6A���A�9�A�e`A��A��MA� �A���A��A�iA���A�FtA� �A�a�A�+A�
�A�{JA��A�5A��A��A�!�A�]dA� 'A���A�`BA���A���A��tA���A���A�H�A��A���Ay�YAm�kAk�AiL0Af�KAc33A^OA\ �AYZAW2�AU��AT�[AR��APR�AM �AKN<AJ%�AGA AE��AC�&AB�AC&�ADX�AC��A>�A=;dA<=�A;f�A;�A;xlA;��A;�.A:��A8�KA61'A5�A4�"A1��A0��A.�A-
�A-;A,�>A)�mA(��A'!�A%zA#�gA!�A!�A �eA�A6zA�}A�:A��A��A7�A+kA�<A*0A�$AzA�A��A �A��AZA֡AYKA�mAD�A�&Ag�AqA4APHAA��A�ACA�DA��A0UAn/AR�A�HA�Aw�AA��A��A�.AFtA��A(�A
��A
OA	�A	�}A	aA	.IA	($A�8A�)A�eA&�A��Ac�A%FA��AqA(Ae�A/A�.A� A�#A@OA�bA4�AƨAV�A�A ��A ��A xlA M@�A�@��?@�+�@�w�@� �@���@�k�@�F�@���@�n/@�A�@��@���@���@���@�\�@���@�ȴ@�-�@�@��@��@�@�@�Q�@��@�7L@�u�@�Z@��@�a�@�B�@�33@���@�\�@��@�E�@�ݘ@��@�9X@��@��d@��|@�p;@�1@���@崢@�4�@���@���@�$�@�W?@��I@�4@ߓ�@���@�c @ފr@�	@��+@ݬq@��>@��@��@�}�@�+@��@���@��@�U2@ۥ@�}�@�8�@ڥz@��@���@ف�@��@ؾ�@؃@ח�@֏\@�خ@�e�@��@��@�͟@�z�@�2�@�a@Һ�@�`�@�~@��}@��@�GE@��)@ϧ�@ϵt@��'@�z�@΄�@�~(@�p;@�7@��N@�RT@��`@�n�@�c @˲-@ʱ�@�I�@���@ɖS@�A @ȵ�@�r�@�,=@ǝ�@�"�@ƌ@�g8@�0U@�
�@���@ńM@��)@���@�c�@�ی@�ff@��@���@�S�@�?}@���@�p;@�e@��m@���@��@���@���@�J#@��@��)@�g�@�@��6@�m�@�=q@��@�ϫ@���@�Mj@�&@���@���@�e@��P@�,�@��9@�C�@��@���@��k@�dZ@��@�M@�e,@� \@���@�\�@�'R@���@�C@�]d@�e@�G@��D@���@�=@���@�-�@�J@��@��
@���@�T�@��@���@��@��@�w�@�kQ@�  @���@�w2@�o�@�\�@�%F@���@���@���@��R@�y>@��@�O�@�q@���@�>B@���@��:@�U�@��@�]d@�7�@�
�@��S@�m]@�C�@��@��@��2@��x@�x@�ԕ@���@�'�@�C@��@��u@��>@���@�x�@�W?@�*0@��@��U@�l�@�G@�@���@�U�@�>�@�@���@�($@���@���@�o�@��@���@�YK@���@���@�^�@��9@�_@�$@���@�x@�'�@�$t@��4@�W�@���@��*@�k�@�ѷ@�A�@��@��:@�!-@�Ĝ@���@���@�kQ@��@��a@�X@�4�@�@@��@�u@��;@��w@��7@�0�@��@�tT@��@�خ@��S@��@��@��o@�?@��@��]@��@�� @���@��	@��9@�H�@�M@�� @�e,@���@��j@��$@�Z�@�kQ@���@�(�@��D@��'@�Y@���@�Z@�)�@��n@�o@�͟@��!@�I�@�@���@�(@��.@�A�@�C�@�H�@�=q@�)�@��@���@�e�@�>�@�
=@���@��$@�Ta@��@��@�v`@�	l@��@���@�}V@�1'@�b@�_@���@���@��d@��a@��C@��V@��	@���@�v`@�F�@��@��	@���@�1�@�@��@&@~�h@~Ov@}�j@}o @}?}@}&�@}@@|��@|��@|�D@|H@|�@{�a@{s@{U�@{4�@{+@z��@zH�@z+k@z�@z@y��@ye,@y5�@x��@xA�@w��@wy�@v��@v��@v��@vl�@v �@uj@t�[@t(�@s]�@r�r@rE�@r�@q�H@p�@o�]@o��@o9�@n�@n^5@nu@m��@l�`@lV�@l�@kt�@j�@j�\@i��@i&�@hN�@g�@g�P@g6z@f�R@e��@d�4@d�@c�@aB�@aY�@a��@a�^@a8�@`�D@`I�@_qv@_ i@^͟@^Ov@]rG@\�@\%�@[��@[x@Z��@ZYK@Y��@YT�@Y�@X�|@X�@Xѷ@X�@Xw�@XZ@X@W�@Wx@W1�@W(@V��@V�@V�}@V��@Vi�@V�@U��@U^�@U�@T�4@T��@T�@T-�@S�@Sb�@S_p@SZ�@SW?@S8@R�m@RYK@Q�X@Qw2@Q-w@P��@P��@P��@P��@P`�@O��@N҉@N��@NTa@N�@M��@M��@M&�@L�$@LH@K� @K�4@K�@J�,@J~�@J4@I��@Ihs@H��@H%�@G��@GH�@F��@F��@Fs�@Fd�@F�@E�@E/@D�|@D֡@D�z@DK^@D�@C��@C=@B�!@BC�@A�t@@�@@V�@?��@?��@>��@>	@=�@=��@<�@<��@<A�@;�@;v`@;E9@;&@:�,@:��@:��@:@�@9�#@9��@9^�@9IR@9u�@9Q�@9L�@9L�@9�@9;@8��@8_@7��@7|�@7E9@6�@6�@6h
@5��@5zx@5`B@5*0@4�@4�`@4��@4��@4�u@4tT@4Z@3�@3��@3]�@3�@2~�@2_�@2Ov@2@1�9@1�@1��@1p�@1k�@1[W@1(�@0��@0�Y@0e�@0_@09X@0 �@07@01@/�4@.�]@.��@.�}@.��@.{�@.xl@.q�@.Z�@-��@-��@-��@-Y�@-F@-*0@-�@-@,�f@,�E@,Ɇ@,��@,��@,�@+��@+C�@+�@*��@*L0@)��@)�9@)��@)(�@(��@(g8@(<�@(@'��@'\)@&��@&	@%��@%�S@%e,@%q@$֡@$I�@$�@$7@$M@$�@#��@#��@#J#@"�@"�}@"��@"+k@!��@!u�@!L�@!@ Ĝ@ �.@ m�@ (�@�
@��@j�@X�@�@�8@�@z@Ta@=q@&�@�)@�'@�@Q�@��@S�@-�@�;@ƨ@��@U�@.I@�@�2@�b@_�@_@��@\�@-w@�)@��@�z@��@]d@4n@��@�f@J#@�@�2@��@��@}V@q�@l�@E�@�@�D@�S@-w@��@��@/�@"h@�@�@1@j�@�"@�@��@q�@=q@��@��@|@f�@F@!�@;@�/@�$@y>@V�@H@*�@~@�@�
@�0@�[@��@��@��@��@�4@qv@;d@4�@.I@"�@�@�M@�@�L@��@L0@��@�@p�@Vm@?}@�v@�4@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B�{B�3B�3B��B��B�lB��B��B�!B��B�B�B��B�B�EB%�Bc:Bz�B�vB��B�[B�JB�&B�B	v�B	�PB	ٚB
 'B
QhB
y�B
�|B
�HB
�xB
��BJ�BOvBS�BdBs�B�(B�{B�|B��B�>B�.B�B�OB��B��B�fB�0B��B�LB�B��B �B;B�B	B�B
BB�B�BkB�B�B  B	�BB�B'�B(�B$ZB�B�B�B�DB��B��BуBÖB��B��Bk6B/�B
ڠB
��B
qvB
W�B
B'B
�B	�UB	�yB	\�B	JXB	A�B	88B	*0B	uB	
rB��B��B��B�%B�B�2B��B�QB�SBοB�B�_BΥB��B	%�B	8�B	!�B	qB	qB	"�B	<6B	`BB	~(B	�mB	�^B	�+B	�B	��B	{�B	jB	]�B	O�B	F%B	I�B	KxB	@�B	S�B	M6B	A B	6zB	/ B	FtB	N�B	B�B	S&B	U�B	Z7B	c�B	y	B	��B	�2B	�pB	�B	}�B	~�B	��B	��B	��B	��B	�2B	��B	�MB	�oB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�EB	�bB	��B	�FB	�mB	��B	��B	��B	�:B	��B	��B	�B	�6B	�yB	�*B	��B	��B	�QB	�5B	��B	��B	��B	�|B	�B	�B	�hB	��B	�3B	�GB	�-B	��B	�aB	��B	��B	��B	��B	��B	�B	�eB	��B	��B	�'B	��B	��B	��B	� B	��B	��B	�B	��B	��B	��B	�tB	��B	�XB	��B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�wB	�B	�.B	�]B	��B	�wB	�.B	�VB	�B	�B	��B	��B	��B	��B	�0B	�jB	��B	��B	�B	��B	�-B	�B	�EB	ňB	�B	�-B	�aB	�'B	�oB	�3B	̳B	�VB	ΊB	��B	�[B	��B	��B	�yB	�KB	ٚB	ٚB	ٴB	ڠB	�	B	ڠB	چB	ڠB	�QB	�B	ڠB	�B	�B	�EB	��B	�B	��B	��B	�EB	�_B	�B	ۦB	یB	��B	�KB	��B	�1B	خB	�B	�eB	�eB	�B	��B	ܒB	�OB	��B	�vB	�vB	��B	�vB	�B	�bB	�B	�-B	�NB	��B	�hB	�B	�TB	��B	�&B	�nB	��B	�B	� B	�B	�B	�B	�B	�nB	�nB	�B	��B	�&B	��B	�@B	�zB	��B	�LB	��B	�B	�RB	�B	�B	�B	��B	��B	�B	�yB	�B	�B	�=B	�B	�B	�B	�B	��B	�B	�]B	�wB	��B	��B	�/B	�cB	��B	�B	�UB	�B	��B	��B	�GB	��B	�B	��B	�hB	�B	��B	��B	��B	�B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	�	B	�XB	�DB	��B	�B	�B	��B	�B	�B	�dB	�dB	�B	�6B	�B	�B	�B	�jB	�B	�B	�BB	��B	�(B	��B	�B	�cB	��B	��B	��B	�HB	�.B	��B	�B	��B	��B
  B	��B
 B
 iB
 OB
 �B
UB
�B
�B
AB
uB
[B
[B
�B
-B
-B
B
aB
GB
GB
B
mB
?B
?B
tB
?B
B
B
�B
�B
�B
tB
B
�B
�B
	B
	7B
	7B
	�B
	�B

XB

�B

�B

�B

�B
�B
~B
dB
6B
�B
�B
�B
pB
�B
�B
\B
B
bB
.B
�B
B
NB
�B
B
�B
oB
�B
�B
�B
B
�B
[B
�B
,B
,B
aB
2B
gB
�B
�B
B
B
9B
SB
�B
sB
�B
�B
�B
�B
�B
�B
�B
	B
QB
�B
�B
�B
�B
OB
�B
B
�B
�B
 \B
 \B
 �B
 �B
!HB
 �B
�B
�B
�B
 'B
!bB
"hB
"4B
"hB
#�B
$ZB
$�B
$�B
%FB
%B
%zB
%�B
%�B
%�B
%�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
'�B
(
B
(
B
($B
($B
(>B
(sB
(sB
(�B
(�B
)*B
)B
)yB
)yB
)*B
)*B
)yB
)*B
)�B
)�B
+QB
+�B
,�B
,�B
,�B
-B
-)B
-]B
-wB
-]B
-�B
-�B
-�B
-wB
-�B
./B
.}B
.�B
.�B
/OB
/�B
/�B
/�B
/�B
/�B
0B
0�B
1'B
1AB
1AB
1vB
1�B
2B
2�B
3�B
49B
4nB
49B
4�B
5�B
5tB
5�B
5�B
6+B
6`B
6`B
6zB
72B
7LB
7fB
7�B
88B
8lB
9	B
9	B
9�B
:B
9�B
:^B
:DB
9XB
8�B
7�B
6�B
4nB
5�B
9XB
:xB
:xB
;B
:�B
:*B
9�B
9>B
8�B
8RB
7�B
7�B
7�B
7�B
8�B
8�B
9rB
9�B
:DB
:�B
:�B
:�B
;0B
;B
<B
<�B
="B
=�B
=�B
=�B
=�B
=�B
>B
>B
>B
=�B
=�B
=�B
>BB
?HB
?�B
@OB
AB
AUB
A;B
AoB
AoB
AoB
A�B
BAB
B�B
BuB
BuB
B�B
CB
C-B
CaB
CaB
C�B
D�B
EB
EB
ESB
E�B
E�B
E�B
FYB
F�B
FtB
F%B
F?B
F�B
GB
G�B
H1B
H�B
H�B
H�B
HfB
HfB
H�B
IB
I7B
I7B
IRB
I�B
I�B
JrB
J�B
J�B
J�B
K)B
KDB
K�B
K�B
L�B
L�B
MjB
N<B
N�B
OBB
P}B
Q4B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S[B
SuB
SuB
S�B
TB
T�B
T�B
U�B
V�B
V�B
V�B
V�B
W
B
W?B
V�B
WsB
W�B
W�B
W�B
XEB
XB
W�B
Y1B
ZB
ZB
ZQB
Z�B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[#B
[#B
[WB
[qB
[�B
\]B
\�B
\�B
\xB
\�B
\�B
]B
\�B
]B
]B
\�B
\�B
\�B
]�B
^5B
^B
^B
^5B
^5B
^5B
^B
^5B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
a-B
aHB
aHB
b4B
bhB
b�B
b�B
c:B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
e,B
e,B
e�B
e�B
ffB
f�B
g�B
g�B
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
iDB
iDB
iyB
i�B
i�B
j0B
jB
j�B
kB
kB
kB
k�B
k�B
k�B
lB
l"B
lWB
l=B
l�B
l�B
mB
m]B
n/B
ncB
n}B
n�B
n�B
n�B
o B
oB
oiB
pB
p!B
p;B
pUB
p�B
p�B
p�B
qAB
q[B
q[B
qvB
q�B
q�B
q�B
rGB
r|B
r�B
r�B
s3B
sMB
sMB
shB
sMB
s�B
s�B
s�B
tB
t�B
t�B
u%B
utB
utB
utB
uZB
uZB
vB
vzB
vzB
v�B
v�B
wB
w�B
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
y	B
y$B
y>B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zB
zB
zxB
zxB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{�B
|B
|B
|B
|�B
|�B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B�{B�3B�3B��B��B�lB��B��B�!B��B�B�B��B�B�EB%�Bc:Bz�B�vB��B�[B�JB�&B�B	v�B	�PB	ٚB
 'B
QhB
y�B
�|B
�HB
�xB
��BJ�BOvBS�BdBs�B�(B�{B�|B��B�>B�.B�B�OB��B��B�fB�0B��B�LB�B��B �B;B�B	B�B
BB�B�BkB�B�B  B	�BB�B'�B(�B$ZB�B�B�B�DB��B��BуBÖB��B��Bk6B/�B
ڠB
��B
qvB
W�B
B'B
�B	�UB	�yB	\�B	JXB	A�B	88B	*0B	uB	
rB��B��B��B�%B�B�2B��B�QB�SBοB�B�_BΥB��B	%�B	8�B	!�B	qB	qB	"�B	<6B	`BB	~(B	�mB	�^B	�+B	�B	��B	{�B	jB	]�B	O�B	F%B	I�B	KxB	@�B	S�B	M6B	A B	6zB	/ B	FtB	N�B	B�B	S&B	U�B	Z7B	c�B	y	B	��B	�2B	�pB	�B	}�B	~�B	��B	��B	��B	��B	�2B	��B	�MB	�oB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�EB	�bB	��B	�FB	�mB	��B	��B	��B	�:B	��B	��B	�B	�6B	�yB	�*B	��B	��B	�QB	�5B	��B	��B	��B	�|B	�B	�B	�hB	��B	�3B	�GB	�-B	��B	�aB	��B	��B	��B	��B	��B	�B	�eB	��B	��B	�'B	��B	��B	��B	� B	��B	��B	�B	��B	��B	��B	�tB	��B	�XB	��B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�wB	�B	�.B	�]B	��B	�wB	�.B	�VB	�B	�B	��B	��B	��B	��B	�0B	�jB	��B	��B	�B	��B	�-B	�B	�EB	ňB	�B	�-B	�aB	�'B	�oB	�3B	̳B	�VB	ΊB	��B	�[B	��B	��B	�yB	�KB	ٚB	ٚB	ٴB	ڠB	�	B	ڠB	چB	ڠB	�QB	�B	ڠB	�B	�B	�EB	��B	�B	��B	��B	�EB	�_B	�B	ۦB	یB	��B	�KB	��B	�1B	خB	�B	�eB	�eB	�B	��B	ܒB	�OB	��B	�vB	�vB	��B	�vB	�B	�bB	�B	�-B	�NB	��B	�hB	�B	�TB	��B	�&B	�nB	��B	�B	� B	�B	�B	�B	�B	�nB	�nB	�B	��B	�&B	��B	�@B	�zB	��B	�LB	��B	�B	�RB	�B	�B	�B	��B	��B	�B	�yB	�B	�B	�=B	�B	�B	�B	�B	��B	�B	�]B	�wB	��B	��B	�/B	�cB	��B	�B	�UB	�B	��B	��B	�GB	��B	�B	��B	�hB	�B	��B	��B	��B	�B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	�	B	�XB	�DB	��B	�B	�B	��B	�B	�B	�dB	�dB	�B	�6B	�B	�B	�B	�jB	�B	�B	�BB	��B	�(B	��B	�B	�cB	��B	��B	��B	�HB	�.B	��B	�B	��B	��B
  B	��B
 B
 iB
 OB
 �B
UB
�B
�B
AB
uB
[B
[B
�B
-B
-B
B
aB
GB
GB
B
mB
?B
?B
tB
?B
B
B
�B
�B
�B
tB
B
�B
�B
	B
	7B
	7B
	�B
	�B

XB

�B

�B

�B

�B
�B
~B
dB
6B
�B
�B
�B
pB
�B
�B
\B
B
bB
.B
�B
B
NB
�B
B
�B
oB
�B
�B
�B
B
�B
[B
�B
,B
,B
aB
2B
gB
�B
�B
B
B
9B
SB
�B
sB
�B
�B
�B
�B
�B
�B
�B
	B
QB
�B
�B
�B
�B
OB
�B
B
�B
�B
 \B
 \B
 �B
 �B
!HB
 �B
�B
�B
�B
 'B
!bB
"hB
"4B
"hB
#�B
$ZB
$�B
$�B
%FB
%B
%zB
%�B
%�B
%�B
%�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
'�B
(
B
(
B
($B
($B
(>B
(sB
(sB
(�B
(�B
)*B
)B
)yB
)yB
)*B
)*B
)yB
)*B
)�B
)�B
+QB
+�B
,�B
,�B
,�B
-B
-)B
-]B
-wB
-]B
-�B
-�B
-�B
-wB
-�B
./B
.}B
.�B
.�B
/OB
/�B
/�B
/�B
/�B
/�B
0B
0�B
1'B
1AB
1AB
1vB
1�B
2B
2�B
3�B
49B
4nB
49B
4�B
5�B
5tB
5�B
5�B
6+B
6`B
6`B
6zB
72B
7LB
7fB
7�B
88B
8lB
9	B
9	B
9�B
:B
9�B
:^B
:DB
9XB
8�B
7�B
6�B
4nB
5�B
9XB
:xB
:xB
;B
:�B
:*B
9�B
9>B
8�B
8RB
7�B
7�B
7�B
7�B
8�B
8�B
9rB
9�B
:DB
:�B
:�B
:�B
;0B
;B
<B
<�B
="B
=�B
=�B
=�B
=�B
=�B
>B
>B
>B
=�B
=�B
=�B
>BB
?HB
?�B
@OB
AB
AUB
A;B
AoB
AoB
AoB
A�B
BAB
B�B
BuB
BuB
B�B
CB
C-B
CaB
CaB
C�B
D�B
EB
EB
ESB
E�B
E�B
E�B
FYB
F�B
FtB
F%B
F?B
F�B
GB
G�B
H1B
H�B
H�B
H�B
HfB
HfB
H�B
IB
I7B
I7B
IRB
I�B
I�B
JrB
J�B
J�B
J�B
K)B
KDB
K�B
K�B
L�B
L�B
MjB
N<B
N�B
OBB
P}B
Q4B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S[B
SuB
SuB
S�B
TB
T�B
T�B
U�B
V�B
V�B
V�B
V�B
W
B
W?B
V�B
WsB
W�B
W�B
W�B
XEB
XB
W�B
Y1B
ZB
ZB
ZQB
Z�B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[#B
[#B
[WB
[qB
[�B
\]B
\�B
\�B
\xB
\�B
\�B
]B
\�B
]B
]B
\�B
\�B
\�B
]�B
^5B
^B
^B
^5B
^5B
^5B
^B
^5B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
a-B
aHB
aHB
b4B
bhB
b�B
b�B
c:B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
e,B
e,B
e�B
e�B
ffB
f�B
g�B
g�B
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
iDB
iDB
iyB
i�B
i�B
j0B
jB
j�B
kB
kB
kB
k�B
k�B
k�B
lB
l"B
lWB
l=B
l�B
l�B
mB
m]B
n/B
ncB
n}B
n�B
n�B
n�B
o B
oB
oiB
pB
p!B
p;B
pUB
p�B
p�B
p�B
qAB
q[B
q[B
qvB
q�B
q�B
q�B
rGB
r|B
r�B
r�B
s3B
sMB
sMB
shB
sMB
s�B
s�B
s�B
tB
t�B
t�B
u%B
utB
utB
utB
uZB
uZB
vB
vzB
vzB
v�B
v�B
wB
w�B
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
y	B
y$B
y>B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zB
zB
zxB
zxB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{�B
|B
|B
|B
|�B
|�B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105003  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175649  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175650  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175650                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025657  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025657  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                