CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:42:30Z creation;2022-06-04T17:42:30Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174230  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٸ��8!1   @ٸ�VH,@.���v��cB��O�;1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�33A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB���B�33B�  B�33B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�33B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�<�Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�H@z�H@���A Q�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�BxzB�=pB�p�B�
=B��
B�
=B��
B��
B���B��
B��
B��
B��
B���B��
B��
B�
=B�=pB��
Bǣ�B��
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
B��
C�C�C�C�C	�C�C�CC�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CNCO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D)GD)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�DyGDyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�:>D�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�z>D�>D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��JA���A���A��AA��A���A�ҽA��A��HA��9A��mA���A���A�� Aʾ�Aʼ�AʽAʼAʹXAʸRAʹ$AʴnAʲ�AʯOAʠ�Aʐ�A�}�A�jKA�c A�I�A�_Aɼ�A���A���A���A���A���A��A�]�A���A��{A�F�A�n�A��)A�NpA�O�A�7�A�w�A�7LA�{�A��qA���A��[A�~(A��A�A�A��A���A��A���A��A�!bA��A�_A{�Au��At�ApU2Ak�fAh��AeP�Ac�NAa	A_�FA\�uAY�AW�[AR��AQO�APdZAN�[AK��AI�AJS&AJ)�AI?AG�AE��AC��AAA?�	A=��A;��A9�&A8~�A6��A3�OA0A/��A.@A,CA*��A(��A&*0A$��A$�A"R�A oiA��A��Ao�Ap�Ar�A��A�fA�9A��A`BA_Ag8Al�A�A|A�+A��A)�AA��A�Ay>A:*A�LAc�A�+A�9A�'AoA/�A/A��A��A�fA3�A��A&�A?�Ae�AA��A�KA�>A�A�4A�XAoiA�fA1�A�A��A�.A~(AVA�-A_�A�AL�A�YA
�HA
�A	�BA	RTAd�A�3A?AL�A�fA�A�\Au�AO�A8�A��A͟APHA%A��A8�A$�A�A]dA�CAR�AA��A/�A҉A�qA ��A q�A c�A N�A )�@�W?@�J�@�C�@� �@��M@��f@�]�@�kQ@��r@��9@���@��r@�ԕ@���@�4@� �@�c�@�p;@�!@��@�2�@���@�~(@��@�b@�|@��y@��@�6z@�A@�ƨ@�D�@�U�@�@��@�o @���@��@�:�@��@�_p@�\@��a@�;d@��@�R�@�^@�Ɇ@�`�@��@�\)@��K@�oi@ߪ�@�|�@�X@�8@��@��@�Ɇ@�\�@��;@�o�@� \@��U@ی~@�!�@ڧ�@��@ٲ-@��@�B[@ךk@�(@ְ!@��@Ոf@��@��,@�L0@��z@ӎ�@��K@ғu@��@�A�@Ь�@Т4@А.@�L0@�v`@��f@α�@ͬq@���@̜x@�YK@�~@��d@�u�@�q@���@�*�@�&�@�>B@Ǡ'@�O�@�ff@���@�^�@�>�@�7L@�;d@��/@�<�@é�@�~�@�0�@�q�@�\�@�S@���@���@�l�@�e�@�Q�@�&�@�ƨ@�=@�#�@��@�b@��@@�x@���@���@���@���@�C-@���@�w2@�]�@��@��)@�xl@�C�@��
@���@�c�@�=�@��@�Ĝ@�m�@�	@�g�@��X@�kQ@�	@���@�W?@� i@���@���@�V�@���@�o @��j@��@���@�,�@��@�V@�}�@���@��}@��L@���@�B[@���@�j@���@�ԕ@��M@��U@�h
@��@�_p@��f@�R�@���@�iD@��@��@�z�@�0U@��#@�~�@�	l@��|@�Ɇ@�~�@�_@��9@���@�Mj@���@���@���@��@��M@��m@�|�@�Ov@�>B@���@�Z�@��@���@���@�M@��@���@��@���@��x@�a|@�Z�@�2�@��@��M@�s�@�Vm@�7L@��@��c@���@�u�@�`�@�K^@�!�@��K@��[@�dZ@���@��X@�ߤ@���@�}V@�^5@�4@���@�U�@�(�@��p@�]d@��@��j@��t@���@�a@�@O@�+�@���@���@�\�@��>@���@���@�RT@���@��@���@�g8@� �@��H@�|�@���@���@���@�q@�-�@��;@�o�@��f@��j@�_�@��9@��	@�K�@�+@��@��@��p@�kQ@���@�x�@�H�@�!�@���@���@�l"@�=q@���@��$@�U�@�33@��@�Ɇ@��@�$�@���@���@�s@�F�@�:�@�(�@�+@�S@��|@��6@�Ta@�@��@���@�x�@�o�@�hs@�/@���@��j@�ff@�N�@�1'@��@���@���@���@�^�@�4@��	@�Ĝ@���@�u�@�Q@��@���@���@�y�@�l�@���@��!@�M@�5?@�/�@� �@�
�@�@~!�@}��@}�3@}�~@}/@|�)@|K^@{�@{,�@z��@z��@zZ�@y��@y2a@x��@xh�@x>B@w��@w��@v�c@v-@u�7@uB�@t�@t`�@t�@s��@s(@rM�@q�d@q��@qG�@p�@p��@p�@o�0@oF�@n��@n_�@ne@m��@m�h@l�@k�[@k�@k�	@k>�@j�,@j�\@j+k@i�@i0�@hQ�@g��@gC�@f�@f��@fZ�@f0U@e�@eԕ@e��@e��@es�@eQ�@e;@d��@d/�@c��@cy�@c+@b��@bl�@bE�@a�9@a�C@a�=@ahs@aq@`Ɇ@`M@_�@_b�@^�@^�!@^kQ@^-@]�@]�X@]��@]?}@]	l@\�p@\��@\,=@[��@[� @[j�@[=@Z��@Zp;@Z?@Y��@Yo @X�)@X�@Xy>@X"h@W��@W@O@V��@V��@VH�@V{@U�o@U��@U@@T��@S�;@S|�@SZ�@S�@R�@R@�@Q��@Q8�@P��@P�U@P��@P�@P`�@P�@O��@Og�@N��@N�r@N}V@NYK@M�>@MG�@M�@L�v@L��@Lc�@K�;@KMj@J��@JJ�@J@I�t@IF@I�@H��@H6@H%�@G�g@GRT@F��@Fl�@E��@EJ�@D��@Dz�@DA�@C�Q@Cx@C@BL0@A��@As�@@�p@@>B@@�@?��@?\)@>�@>��@>L0@=�T@=IR@=@@<��@<V�@<�@;��@;��@;l�@;S�@;;d@;)_@;�@:�"@:�<@:��@:u%@:+k@9��@9G�@8��@8�E@8�I@8  @7��@74�@6��@6��@6��@6\�@5�>@5�N@5�3@5�-@5+�@4�Y@3� @3t�@31�@2�@2��@2c @2:*@1�@1��@1Q�@1%F@0�v@0l"@0x@/�w@/j�@/F�@/(@.�@.GE@.�@-�@-�@-(�@,��@,|�@,'R@+�@+��@+a@+1�@*��@*�h@*YK@*_@)�o@)��@(��@(/�@(�@'�@'�0@'�f@'4�@&�m@&p;@&8�@&�@%�o@%��@%T�@%=�@%/@$�f@$��@$��@$r�@#�A@#�F@#��@#x@#�@"�L@"YK@":*@"@!��@!��@!rG@!G�@!+@ U2@��@qv@Z�@H�@�@�]@a|@+k@��@�j@��@x�@S&@�@�P@�`@��@|�@Xy@%�@x@�]@�P@a@>�@�8@�@ں@a|@u@��@�7@hs@2a@�	@�p@�u@w�@Z@A�@�@��@��@_p@�@��@�H@ߤ@�,@��@Q@u@��@��@`B@�)@�Y@|�@j@Ft@,=@'R@�@��@�0@�$@P�@+@Y@�]@��@Ta@@�@3�@($@O@J@�@�^@��@k�@F@�@�)@��@�e@j@I�@7�@*�@�}@x@b�@H�@�@�2@��@�@�}@ff@M�@{@@�@��@zx@f�@J�@!�@�5@��@|�@[�@"h@�@�@�@�6@��@iD@
�8@
��@
�H@
�h@
�F@
��@
}V@
u%@
C�@
!�@	�>@	��@	�@	hs@	O�@	/@�K@�$@��@�Y@9X@  @��@��@�w@��@]�@9�@$t@(@�@��@�b@��@� @xl@d�@L0@8�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��JA���A���A��AA��A���A�ҽA��A��HA��9A��mA���A���A�� Aʾ�Aʼ�AʽAʼAʹXAʸRAʹ$AʴnAʲ�AʯOAʠ�Aʐ�A�}�A�jKA�c A�I�A�_Aɼ�A���A���A���A���A���A��A�]�A���A��{A�F�A�n�A��)A�NpA�O�A�7�A�w�A�7LA�{�A��qA���A��[A�~(A��A�A�A��A���A��A���A��A�!bA��A�_A{�Au��At�ApU2Ak�fAh��AeP�Ac�NAa	A_�FA\�uAY�AW�[AR��AQO�APdZAN�[AK��AI�AJS&AJ)�AI?AG�AE��AC��AAA?�	A=��A;��A9�&A8~�A6��A3�OA0A/��A.@A,CA*��A(��A&*0A$��A$�A"R�A oiA��A��Ao�Ap�Ar�A��A�fA�9A��A`BA_Ag8Al�A�A|A�+A��A)�AA��A�Ay>A:*A�LAc�A�+A�9A�'AoA/�A/A��A��A�fA3�A��A&�A?�Ae�AA��A�KA�>A�A�4A�XAoiA�fA1�A�A��A�.A~(AVA�-A_�A�AL�A�YA
�HA
�A	�BA	RTAd�A�3A?AL�A�fA�A�\Au�AO�A8�A��A͟APHA%A��A8�A$�A�A]dA�CAR�AA��A/�A҉A�qA ��A q�A c�A N�A )�@�W?@�J�@�C�@� �@��M@��f@�]�@�kQ@��r@��9@���@��r@�ԕ@���@�4@� �@�c�@�p;@�!@��@�2�@���@�~(@��@�b@�|@��y@��@�6z@�A@�ƨ@�D�@�U�@�@��@�o @���@��@�:�@��@�_p@�\@��a@�;d@��@�R�@�^@�Ɇ@�`�@��@�\)@��K@�oi@ߪ�@�|�@�X@�8@��@��@�Ɇ@�\�@��;@�o�@� \@��U@ی~@�!�@ڧ�@��@ٲ-@��@�B[@ךk@�(@ְ!@��@Ոf@��@��,@�L0@��z@ӎ�@��K@ғu@��@�A�@Ь�@Т4@А.@�L0@�v`@��f@α�@ͬq@���@̜x@�YK@�~@��d@�u�@�q@���@�*�@�&�@�>B@Ǡ'@�O�@�ff@���@�^�@�>�@�7L@�;d@��/@�<�@é�@�~�@�0�@�q�@�\�@�S@���@���@�l�@�e�@�Q�@�&�@�ƨ@�=@�#�@��@�b@��@@�x@���@���@���@���@�C-@���@�w2@�]�@��@��)@�xl@�C�@��
@���@�c�@�=�@��@�Ĝ@�m�@�	@�g�@��X@�kQ@�	@���@�W?@� i@���@���@�V�@���@�o @��j@��@���@�,�@��@�V@�}�@���@��}@��L@���@�B[@���@�j@���@�ԕ@��M@��U@�h
@��@�_p@��f@�R�@���@�iD@��@��@�z�@�0U@��#@�~�@�	l@��|@�Ɇ@�~�@�_@��9@���@�Mj@���@���@���@��@��M@��m@�|�@�Ov@�>B@���@�Z�@��@���@���@�M@��@���@��@���@��x@�a|@�Z�@�2�@��@��M@�s�@�Vm@�7L@��@��c@���@�u�@�`�@�K^@�!�@��K@��[@�dZ@���@��X@�ߤ@���@�}V@�^5@�4@���@�U�@�(�@��p@�]d@��@��j@��t@���@�a@�@O@�+�@���@���@�\�@��>@���@���@�RT@���@��@���@�g8@� �@��H@�|�@���@���@���@�q@�-�@��;@�o�@��f@��j@�_�@��9@��	@�K�@�+@��@��@��p@�kQ@���@�x�@�H�@�!�@���@���@�l"@�=q@���@��$@�U�@�33@��@�Ɇ@��@�$�@���@���@�s@�F�@�:�@�(�@�+@�S@��|@��6@�Ta@�@��@���@�x�@�o�@�hs@�/@���@��j@�ff@�N�@�1'@��@���@���@���@�^�@�4@��	@�Ĝ@���@�u�@�Q@��@���@���@�y�@�l�@���@��!@�M@�5?@�/�@� �@�
�@�@~!�@}��@}�3@}�~@}/@|�)@|K^@{�@{,�@z��@z��@zZ�@y��@y2a@x��@xh�@x>B@w��@w��@v�c@v-@u�7@uB�@t�@t`�@t�@s��@s(@rM�@q�d@q��@qG�@p�@p��@p�@o�0@oF�@n��@n_�@ne@m��@m�h@l�@k�[@k�@k�	@k>�@j�,@j�\@j+k@i�@i0�@hQ�@g��@gC�@f�@f��@fZ�@f0U@e�@eԕ@e��@e��@es�@eQ�@e;@d��@d/�@c��@cy�@c+@b��@bl�@bE�@a�9@a�C@a�=@ahs@aq@`Ɇ@`M@_�@_b�@^�@^�!@^kQ@^-@]�@]�X@]��@]?}@]	l@\�p@\��@\,=@[��@[� @[j�@[=@Z��@Zp;@Z?@Y��@Yo @X�)@X�@Xy>@X"h@W��@W@O@V��@V��@VH�@V{@U�o@U��@U@@T��@S�;@S|�@SZ�@S�@R�@R@�@Q��@Q8�@P��@P�U@P��@P�@P`�@P�@O��@Og�@N��@N�r@N}V@NYK@M�>@MG�@M�@L�v@L��@Lc�@K�;@KMj@J��@JJ�@J@I�t@IF@I�@H��@H6@H%�@G�g@GRT@F��@Fl�@E��@EJ�@D��@Dz�@DA�@C�Q@Cx@C@BL0@A��@As�@@�p@@>B@@�@?��@?\)@>�@>��@>L0@=�T@=IR@=@@<��@<V�@<�@;��@;��@;l�@;S�@;;d@;)_@;�@:�"@:�<@:��@:u%@:+k@9��@9G�@8��@8�E@8�I@8  @7��@74�@6��@6��@6��@6\�@5�>@5�N@5�3@5�-@5+�@4�Y@3� @3t�@31�@2�@2��@2c @2:*@1�@1��@1Q�@1%F@0�v@0l"@0x@/�w@/j�@/F�@/(@.�@.GE@.�@-�@-�@-(�@,��@,|�@,'R@+�@+��@+a@+1�@*��@*�h@*YK@*_@)�o@)��@(��@(/�@(�@'�@'�0@'�f@'4�@&�m@&p;@&8�@&�@%�o@%��@%T�@%=�@%/@$�f@$��@$��@$r�@#�A@#�F@#��@#x@#�@"�L@"YK@":*@"@!��@!��@!rG@!G�@!+@ U2@��@qv@Z�@H�@�@�]@a|@+k@��@�j@��@x�@S&@�@�P@�`@��@|�@Xy@%�@x@�]@�P@a@>�@�8@�@ں@a|@u@��@�7@hs@2a@�	@�p@�u@w�@Z@A�@�@��@��@_p@�@��@�H@ߤ@�,@��@Q@u@��@��@`B@�)@�Y@|�@j@Ft@,=@'R@�@��@�0@�$@P�@+@Y@�]@��@Ta@@�@3�@($@O@J@�@�^@��@k�@F@�@�)@��@�e@j@I�@7�@*�@�}@x@b�@H�@�@�2@��@�@�}@ff@M�@{@@�@��@zx@f�@J�@!�@�5@��@|�@[�@"h@�@�@�@�6@��@iD@
�8@
��@
�H@
�h@
�F@
��@
}V@
u%@
C�@
!�@	�>@	��@	�@	hs@	O�@	/@�K@�$@��@�Y@9X@  @��@��@�w@��@]�@9�@$t@(@�@��@�b@��@� @xl@d�@L0@8�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�hB��B��B��B��B�B�B�B��B�B��B�B��B��B��B��B��B��B�hB�3B�MB��B�aB�-B�vB��B��B��B�B��B�B�[B	̳B
��B
��B
��B
̈́B
ѝB
�jB
�UB
�B
�B
��B
��B
�wB
��B
�DB
�HB
�LB
��B
�xB
��B
��B
�;B
�KB
ӏB
��B
��B
�+B
�UB
��B
u%B
G+B
�B	ܬB	��B	�3B	��B	��B	yrB	e�B	W�B	OvB	G�B	=qB	.�B	(>B	eB	B	B	�B��B��B	
#B	7B	xB	�B	B	
�B	fB	�B	�B	$B	B	
#B	�B	�B�0B	B	�B	~B	B��B�B�:B�'B�,B��BοBοBЗB��BѝB��B		B	)�B	,�B	!�B	�B	�B	EB	�B	&�B	BB	FYB	C�B	LJB	OB	JXB	J=B	TaB	l"B	�&B	�B	�'B	��B	�B	��B	�	B	��B	�}B	�RB	�'B	֡B	�@B	�B	��B	�|B	�VB	՛B	ؓB	��B	�_B	ԕB	��B	�xB	�_B	��B	��B	żB	�+B	��B	āB	�uB	�-B	�oB	��B	��B	��B	��B	��B	��B	�=B	��B	�HB	��B	�LB	��B	�8B	�B	��B	��B	�B	��B	�MB	�XB	�B	уB	�2B	ԯB	ӏB	��B	�+B	�_B	׍B	֡B	׍B	�EB	�aB	�B	��B	��B	ѝB	��B	�B	͟B	ԕB	�~B	�'B	�jB	��B	�B	өB	��B	�dB	��B	�B	�B	�B	�bB	�vB	�B	��B	��B	�=B	�B	�B	�B	�CB	��B	��B	�WB	�eB	��B	��B	�B	�B	��B	��B	�B	�>B	�XB	�B	�*B	�B	�_B	�*B	�0B	�eB	�eB	��B	�B	��B	�eB	�B	��B	�IB	� B	�B	��B	��B	�;B	��B	�B	��B	�B	�B	�hB	�B	�|B	�vB	�B	�B	�-B	�GB	��B	�B	��B	��B	�3B	��B	�B	�B	�B	�B	��B	��B	�%B	�tB	�ZB	�ZB	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�MB	�aB	��B	��B	��B	��B	�B	�cB	��B	�B	�[B	�AB	�B	�oB	�|B	�AB	��B	��B	�|B	�9B	�nB	�9B	�tB	�LB	�B	�lB	�8B	��B	��B	�^B	�0B	�B	��B	��B	��B	�6B	��B	��B	��B	�"B	�<B	�<B	�qB	��B	��B	�B	�B	�(B	�]B	�BB	�]B	��B	�B	�B	�.B	�B	�.B	�.B	�B	�}B	��B	��B	��B	�}B	�}B
 4B
 4B
  B	�cB	��B	�B	��B	��B
�B
AB
uB
�B
GB
-B
GB
{B
AB
B
oB
;B
 �B
 �B
 OB
 B
 B
[B
AB
B
�B
SB
�B
�B
%B
�B
SB
�B
tB
?B
�B
�B
�B
�B
%B
�B
	lB

=B
	�B
	lB
	RB
	B
	RB
	�B

	B

�B

XB

XB
	�B

#B

�B

rB

rB

�B
)B
�B
�B
pB
�B
�B
4B
�B
�B
�B
oB
B
&B
�B
�B
9B
�B
$B

B
�B
EB
EB
�B
�B
YB
$B
?B
+B
�B
7B
�B
�B
WB
WB
�B
CB
)B
)B
�B
kB
�B
/B
�B
B
/B
�B
dB
OB
�B
�B
;B
�B
 'B
 �B
!B
!�B
"hB
"�B
# B
#B
#:B
#B
#�B
#�B
$&B
$ZB
$�B
$�B
%B
%zB
%�B
%�B
&fB
&�B
'B
'B
'8B
'mB
'�B
'�B
(>B
(sB
(�B
(�B
(�B
(�B
(�B
(�B
)B
)yB
*0B
*�B
*�B
+kB
+kB
+QB
+6B
+�B
+�B
+�B
,B
+�B
,qB
,�B
,�B
,�B
-]B
.B
.cB
.�B
/iB
/iB
/�B
0!B
0!B
0B
0�B
1B
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
33B
3hB
3�B
3�B
3�B
4B
49B
49B
4nB
5�B
6+B
5�B
6`B
6�B
6�B
7�B
8B
8�B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:DB
:*B
:^B
:�B
:�B
:�B
;JB
;JB
;�B
<PB
<�B
<jB
<jB
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
>�B
>�B
?B
?�B
?�B
@B
@4B
@�B
@�B
@�B
@�B
AB
AB
A B
AB
A;B
AUB
A�B
A�B
B'B
B[B
BuB
B�B
B�B
CB
CaB
C{B
CaB
C�B
C�B
C�B
D�B
D�B
EB
E9B
EmB
E�B
E�B
FB
F%B
F%B
F�B
F�B
F�B
F�B
G+B
GEB
GEB
G�B
G�B
H1B
HKB
H1B
H�B
H�B
IlB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
J�B
K)B
J�B
K^B
K�B
K�B
L�B
L�B
L�B
MB
MB
M�B
M�B
N<B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
O\B
O�B
PB
O�B
PB
PHB
P�B
Q B
P�B
Q B
QB
Q�B
Q�B
RB
R�B
R�B
R�B
S@B
S&B
S[B
TB
S�B
T,B
TaB
T�B
UB
U�B
U�B
VB
VmB
V�B
V�B
W?B
WsB
XB
X_B
X_B
X�B
Y1B
YeB
YeB
Y�B
ZB
Z7B
ZQB
Z�B
[�B
[�B
[�B
\xB
\�B
\�B
]/B
]IB
]IB
]~B
]dB
]dB
]dB
]�B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
_;B
_�B
`'B
`BB
`BB
`vB
`�B
abB
a-B
a-B
a-B
a�B
bNB
b�B
b�B
c B
c B
cB
cTB
cTB
c�B
c�B
dB
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
fB
ffB
f�B
f�B
g8B
g8B
g�B
g�B
g�B
h
B
hsB
h>B
h�B
h�B
h�B
iB
i�B
jB
i�B
jKB
jeB
jeB
j�B
kQB
k�B
k�B
k�B
k�B
l=B
lqB
lqB
lqB
l�B
l�B
l�B
mB
mwB
m�B
mwB
m�B
nB
nIB
n�B
n�B
n�B
oB
oB
oOB
oiB
oOB
pUB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
r-B
rB
raB
r�B
r|B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
y>B
y�B
y�B
z*B
z*B
zB
zDB
zDB
z�B
z�B
z�B
z�B
{B
{B
{B
{dB
{�B
{�B
|B
|B
|6B
|B
|B
|6B
|�B
|�B
|�B
|�B
}"B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
cB
}B
�B
�B
�B
�B
�B
�B
�OB
��B
��B
��B
�B
�B
�;B
� B
�;B
�UB
�;B
�UB
��B
�B
�B
��B
�[B
�[B
�uB
��B
��B
��B
�-B
�GB
�{B
�{B
��B
��B
�B
�MB
�gB
��B
��B
�9B
�9B
�9B
�SB
��B
��B
��B
�%B
�%B
�?B
�tB
��B
��B
��B
��B
�B
�B
�+B
�B
�E111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�hB��B��B��B��B�B�B�B��B�B��B�B��B��B��B��B��B��B�hB�3B�MB��B�aB�-B�vB��B��B��B�B��B�B�[B	̳B
��B
��B
��B
̈́B
ѝB
�jB
�UB
�B
�B
��B
��B
�wB
��B
�DB
�HB
�LB
��B
�xB
��B
��B
�;B
�KB
ӏB
��B
��B
�+B
�UB
��B
u%B
G+B
�B	ܬB	��B	�3B	��B	��B	yrB	e�B	W�B	OvB	G�B	=qB	.�B	(>B	eB	B	B	�B��B��B	
#B	7B	xB	�B	B	
�B	fB	�B	�B	$B	B	
#B	�B	�B�0B	B	�B	~B	B��B�B�:B�'B�,B��BοBοBЗB��BѝB��B		B	)�B	,�B	!�B	�B	�B	EB	�B	&�B	BB	FYB	C�B	LJB	OB	JXB	J=B	TaB	l"B	�&B	�B	�'B	��B	�B	��B	�	B	��B	�}B	�RB	�'B	֡B	�@B	�B	��B	�|B	�VB	՛B	ؓB	��B	�_B	ԕB	��B	�xB	�_B	��B	��B	żB	�+B	��B	āB	�uB	�-B	�oB	��B	��B	��B	��B	��B	��B	�=B	��B	�HB	��B	�LB	��B	�8B	�B	��B	��B	�B	��B	�MB	�XB	�B	уB	�2B	ԯB	ӏB	��B	�+B	�_B	׍B	֡B	׍B	�EB	�aB	�B	��B	��B	ѝB	��B	�B	͟B	ԕB	�~B	�'B	�jB	��B	�B	өB	��B	�dB	��B	�B	�B	�B	�bB	�vB	�B	��B	��B	�=B	�B	�B	�B	�CB	��B	��B	�WB	�eB	��B	��B	�B	�B	��B	��B	�B	�>B	�XB	�B	�*B	�B	�_B	�*B	�0B	�eB	�eB	��B	�B	��B	�eB	�B	��B	�IB	� B	�B	��B	��B	�;B	��B	�B	��B	�B	�B	�hB	�B	�|B	�vB	�B	�B	�-B	�GB	��B	�B	��B	��B	�3B	��B	�B	�B	�B	�B	��B	��B	�%B	�tB	�ZB	�ZB	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�MB	�aB	��B	��B	��B	��B	�B	�cB	��B	�B	�[B	�AB	�B	�oB	�|B	�AB	��B	��B	�|B	�9B	�nB	�9B	�tB	�LB	�B	�lB	�8B	��B	��B	�^B	�0B	�B	��B	��B	��B	�6B	��B	��B	��B	�"B	�<B	�<B	�qB	��B	��B	�B	�B	�(B	�]B	�BB	�]B	��B	�B	�B	�.B	�B	�.B	�.B	�B	�}B	��B	��B	��B	�}B	�}B
 4B
 4B
  B	�cB	��B	�B	��B	��B
�B
AB
uB
�B
GB
-B
GB
{B
AB
B
oB
;B
 �B
 �B
 OB
 B
 B
[B
AB
B
�B
SB
�B
�B
%B
�B
SB
�B
tB
?B
�B
�B
�B
�B
%B
�B
	lB

=B
	�B
	lB
	RB
	B
	RB
	�B

	B

�B

XB

XB
	�B

#B

�B

rB

rB

�B
)B
�B
�B
pB
�B
�B
4B
�B
�B
�B
oB
B
&B
�B
�B
9B
�B
$B

B
�B
EB
EB
�B
�B
YB
$B
?B
+B
�B
7B
�B
�B
WB
WB
�B
CB
)B
)B
�B
kB
�B
/B
�B
B
/B
�B
dB
OB
�B
�B
;B
�B
 'B
 �B
!B
!�B
"hB
"�B
# B
#B
#:B
#B
#�B
#�B
$&B
$ZB
$�B
$�B
%B
%zB
%�B
%�B
&fB
&�B
'B
'B
'8B
'mB
'�B
'�B
(>B
(sB
(�B
(�B
(�B
(�B
(�B
(�B
)B
)yB
*0B
*�B
*�B
+kB
+kB
+QB
+6B
+�B
+�B
+�B
,B
+�B
,qB
,�B
,�B
,�B
-]B
.B
.cB
.�B
/iB
/iB
/�B
0!B
0!B
0B
0�B
1B
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
33B
3hB
3�B
3�B
3�B
4B
49B
49B
4nB
5�B
6+B
5�B
6`B
6�B
6�B
7�B
8B
8�B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:DB
:*B
:^B
:�B
:�B
:�B
;JB
;JB
;�B
<PB
<�B
<jB
<jB
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
>�B
>�B
?B
?�B
?�B
@B
@4B
@�B
@�B
@�B
@�B
AB
AB
A B
AB
A;B
AUB
A�B
A�B
B'B
B[B
BuB
B�B
B�B
CB
CaB
C{B
CaB
C�B
C�B
C�B
D�B
D�B
EB
E9B
EmB
E�B
E�B
FB
F%B
F%B
F�B
F�B
F�B
F�B
G+B
GEB
GEB
G�B
G�B
H1B
HKB
H1B
H�B
H�B
IlB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
J�B
K)B
J�B
K^B
K�B
K�B
L�B
L�B
L�B
MB
MB
M�B
M�B
N<B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
O\B
O�B
PB
O�B
PB
PHB
P�B
Q B
P�B
Q B
QB
Q�B
Q�B
RB
R�B
R�B
R�B
S@B
S&B
S[B
TB
S�B
T,B
TaB
T�B
UB
U�B
U�B
VB
VmB
V�B
V�B
W?B
WsB
XB
X_B
X_B
X�B
Y1B
YeB
YeB
Y�B
ZB
Z7B
ZQB
Z�B
[�B
[�B
[�B
\xB
\�B
\�B
]/B
]IB
]IB
]~B
]dB
]dB
]dB
]�B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
_;B
_�B
`'B
`BB
`BB
`vB
`�B
abB
a-B
a-B
a-B
a�B
bNB
b�B
b�B
c B
c B
cB
cTB
cTB
c�B
c�B
dB
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
fB
ffB
f�B
f�B
g8B
g8B
g�B
g�B
g�B
h
B
hsB
h>B
h�B
h�B
h�B
iB
i�B
jB
i�B
jKB
jeB
jeB
j�B
kQB
k�B
k�B
k�B
k�B
l=B
lqB
lqB
lqB
l�B
l�B
l�B
mB
mwB
m�B
mwB
m�B
nB
nIB
n�B
n�B
n�B
oB
oB
oOB
oiB
oOB
pUB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
r-B
rB
raB
r�B
r|B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
y>B
y�B
y�B
z*B
z*B
zB
zDB
zDB
z�B
z�B
z�B
z�B
{B
{B
{B
{dB
{�B
{�B
|B
|B
|6B
|B
|B
|6B
|�B
|�B
|�B
|�B
}"B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
cB
}B
�B
�B
�B
�B
�B
�B
�OB
��B
��B
��B
�B
�B
�;B
� B
�;B
�UB
�;B
�UB
��B
�B
�B
��B
�[B
�[B
�uB
��B
��B
��B
�-B
�GB
�{B
�{B
��B
��B
�B
�MB
�gB
��B
��B
�9B
�9B
�9B
�SB
��B
��B
��B
�%B
�%B
�?B
�tB
��B
��B
��B
��B
�B
�B
�+B
�B
�E111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104929  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174230  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174230  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174230                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024238  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024238  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                