CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-09T00:35:29Z creation;2016-09-09T00:35:32Z conversion to V3.1;2019-12-19T08:30:54Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20160909003529  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               #A   JA  I2_0576_035                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��u��� 1   @��v\�$ @;6_ح���di�2�W�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D���D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @'�@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B@{BG�BOG�BW�B_�BgG�Bo�Bw�B�B��
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
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D#HD#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DQHDQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DV�HDV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��=D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��=D�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�@�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��;A�ȴA�ĜA�AѾwAѾwAѾwAѼjAѾwAѼjAѼjAѺ^AѲ-AѮAѧ�Aѕ�A�\)A�I�A��A�JA���A���A��A˗�A��A��A��A���A���A���A��;A�bA�VA��A�bNA�oA�9XA�K�A��A��A�"�A��A���A���A�`BA�A�Q�A�r�A��!A�-A�A�+A�t�A��
A���A�t�A��PA�jA�ƨA�ffA�/A�{A�A�G�A���A��A��RA�+A���A�VA�ƨA���A���A�C�A��;A��A�l�A��\A��/A��#A��A��/A�hsA�9XA�|�AoA{l�Ay��Ay�Axr�Aw33Au%Atn�As�AsO�ArE�Aq��Ap�DAo%An9XAn$�AmAk�#Aj�AhAf�9Ae�7Ad(�Ac
=AbbNAaO�A_��A^z�A\ȴA[�7AZQ�AXz�AWoAV^5AU��ATn�AS�wAR�ARZAR �AQ��AP��AO�AN=qAN{AM33AL�/AL�9AL��AL�AK/AJr�AJ{AJ  AI�AI7LAH�9AG�AE�7AD�AB�/AB-AAhsA@A?"�A>�/A=��A=�A<�A<(�A;�7A;"�A:��A:bNA9��A8��A7��A6�RA6^5A5�A5p�A4(�A3�A2��A29XA1`BA0�A/�FA.��A.z�A.bA-33A,��A,VA,  A+x�A*v�A(��A(jA(ZA($�A'��A&�!A%hsA$��A#A"��A"ffA"1A!�-A!7LA ZA��AAffA�A�/AI�A��A%A�AG�A�9A{AO�AVA-A��AĜA&�A�Av�A�A��A�jA��AI�A��A1'A
��A
��A
1'A��A��A�A�A�Ar�A�FA`BA&�A�A�`A�A��A ��@�%@��y@�=q@��9@�dZ@��R@�ff@�hs@�9X@��m@��y@�v�@�-@���@��/@�@�
=@���@��T@�bN@�@�`B@���@�+@�~�@�E�@�@���@���@㕁@�$�@�$�@���@��@ٲ-@��@�ƨ@�$�@�r�@ӥ�@�V@�Z@ϥ�@�ȴ@�{@ͺ^@�X@�1'@��H@�?}@�  @��@�E�@š�@�/@ă@��@���@��@�A�@�V@��^@�/@���@��@�=q@�@��@��@��-@���@� �@�;d@��-@��7@��@�"�@���@��@���@�&�@��j@��@���@�@��@�Q�@�|�@�dZ@�\)@�\)@�b@��h@���@�v�@�dZ@�;d@�;d@�;d@�C�@�\)@�C�@��@���@��
@��@�  @�;d@�"�@��@���@��+@�E�@�=q@���@�r�@��@��@�o@��y@��!@�=q@�hs@���@��@�;d@�V@�n�@�v�@�n�@�^5@�-@�@��@���@���@�p�@�X@�O�@�G�@�G�@�?}@���@��9@�A�@��;@�;d@�@�l�@���@���@��;@�33@��!@�J@���@���@�@�x�@�7L@��@�bN@�b@��@�S�@�ff@�-@���@��^@�z�@��@��w@��P@�|�@�|�@�l�@�\)@�K�@�33@�"�@��!@�$�@�hs@�?}@��@�V@���@��u@�1@��;@��;@�K�@�"�@��+@��T@��@�r�@��!@�E�@�J@��-@�p�@�/@��@�%@���@��9@��@��D@�bN@�Q�@�1'@��@�  @��@�@~ff@~@~��@~{@}�@|j@{�@{�@{S�@z�H@z�!@z�@y��@yhs@yG�@y%@xQ�@x�9@xbN@x �@w;d@w
=@w�P@w�@w��@up�@t�@t(�@rn�@q�@r=q@r�!@s@so@sS�@s"�@sC�@s�@sS�@r�H@s�
@t�@sƨ@s@r�H@sC�@sS�@sS�@s"�@r�H@q�@p�@p�u@p��@p�9@p�9@p�u@pQ�@o�;@o+@n��@n��@m�@mO�@lz�@l�@k�m@kS�@j~�@jJ@i�#@iX@ihs@iX@i%@h��@h1'@g
=@e�T@e@e�-@e�-@e�-@e�@d(�@c33@b=q@b�@a�7@`��@`�u@`1'@`  @_�@`Q�@a&�@a%@`��@`Q�@_��@_|�@_l�@_K�@_
=@^�@^�R@^��@^�+@^v�@^v�@^v�@^ff@^5?@]@]?}@\�j@\j@\Z@\Z@\Z@\9X@\�@[t�@[33@Z�@Z��@Z��@Z~�@Z^5@Z=q@Z�@Yhs@YG�@Y&�@Y&�@Y�@Y%@X��@X��@XbN@Xb@W�@Wl�@W\)@V��@V��@V@Up�@T��@TI�@S�
@S��@S"�@R�H@R��@R�!@R^5@Q��@Qhs@Q�@P�`@PĜ@P�@PbN@PQ�@P1'@Pb@O�;@OK�@Nȴ@Nv�@N$�@M�@M�-@M?}@L��@L�D@L9X@K�m@KS�@J�@J��@Jn�@I�@I��@I&�@H�u@Hb@Gl�@G
=@Fȴ@F��@FV@E�h@E?}@EV@D��@D�@D��@D�j@D9X@C�m@C�m@C�m@C�m@C�
@C�F@CS�@B�!@A�@A��@A�7@A��@A�7@Ax�@Ax�@Ahs@AG�@@��@@��@@Ĝ@@�9@@A�@@b@?�@?l�@?K�@?�@>��@>�y@>E�@=@=p�@=V@<I�@;��@;��@;��@;��@;��@;�@;dZ@:�H@:^5@:M�@:�@9��@9�#@9�^@9��@9��@9x�@9G�@8��@8Ĝ@8��@8r�@81'@7��@7|�@7
=@5@5�@4��@4��@5�@5V@4j@4(�@4�@3��@3�F@3�@3o@2��@2��@2�\@2~�@2=q@1��@1��@0��@0�u@0r�@/��@/�P@/|�@/K�@/�@.��@.v�@.@.@-�-@-p�@-`B@-?}@-�@-�@,��@,�@,��@,��@,z�@,I�@,1@+��@+ƨ@+dZ@*��@*n�@*-@)��@)��@)7L@(��@(��@(��@(�9@(�u@(�@(�@(r�@(Q�@( �@(b@'�;@'��@'l�@'\)@';d@'+@'�@&�y@&��@&5?@%�@%��@%?}@$��@$�j@$z�@$(�@#ƨ@#��@#dZ@"�@"��@"n�@"M�@"-@!��@!hs@!&�@!%@ ��@ �9@ ��@ bN@ Q�@ b@��@�w@�@�@��@K�@+@
=@
=@ȴ@�+@v�@V@$�@�@��@�h@p�@`B@?}@�@�D@Z@(�@�
@�@33@�@��@�@�!@~�@n�@^5@=q@��@��@�7@&�@r�@�@��@�w@l�@+@�y@$�@�-@?}@�D@�@1@1@1@1@��@�F@33@�!@=q@J@�#@��@��@hs@7L@&�@�@��@�`@��@�u@b@�@\)@��@�R@v�@V@E�@5?@$�@$�@$�@{@��@p�@?}@�@�j@j@(�@1@�m@�F@��@t�@C�@
�@
�!@
~�@
^5@
�@	�#@	�^@	x�@	G�@�`@Ĝ@Ĝ@Ĝ@r�@A�@�@�@�P@l�@\)@;d@;d@�@�@��@ff@�T@�-@��@p�@?}@�@��@�/@��@z�@I�@�@��@��@1@�
@�
@ƨ@��@t�@S�@"�@�@��@�!@��@�\@n�@^5@M�@=q@-@�@J@�@�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��;A�ȴA�ĜA�AѾwAѾwAѾwAѼjAѾwAѼjAѼjAѺ^AѲ-AѮAѧ�Aѕ�A�\)A�I�A��A�JA���A���A��A˗�A��A��A��A���A���A���A��;A�bA�VA��A�bNA�oA�9XA�K�A��A��A�"�A��A���A���A�`BA�A�Q�A�r�A��!A�-A�A�+A�t�A��
A���A�t�A��PA�jA�ƨA�ffA�/A�{A�A�G�A���A��A��RA�+A���A�VA�ƨA���A���A�C�A��;A��A�l�A��\A��/A��#A��A��/A�hsA�9XA�|�AoA{l�Ay��Ay�Axr�Aw33Au%Atn�As�AsO�ArE�Aq��Ap�DAo%An9XAn$�AmAk�#Aj�AhAf�9Ae�7Ad(�Ac
=AbbNAaO�A_��A^z�A\ȴA[�7AZQ�AXz�AWoAV^5AU��ATn�AS�wAR�ARZAR �AQ��AP��AO�AN=qAN{AM33AL�/AL�9AL��AL�AK/AJr�AJ{AJ  AI�AI7LAH�9AG�AE�7AD�AB�/AB-AAhsA@A?"�A>�/A=��A=�A<�A<(�A;�7A;"�A:��A:bNA9��A8��A7��A6�RA6^5A5�A5p�A4(�A3�A2��A29XA1`BA0�A/�FA.��A.z�A.bA-33A,��A,VA,  A+x�A*v�A(��A(jA(ZA($�A'��A&�!A%hsA$��A#A"��A"ffA"1A!�-A!7LA ZA��AAffA�A�/AI�A��A%A�AG�A�9A{AO�AVA-A��AĜA&�A�Av�A�A��A�jA��AI�A��A1'A
��A
��A
1'A��A��A�A�A�Ar�A�FA`BA&�A�A�`A�A��A ��@�%@��y@�=q@��9@�dZ@��R@�ff@�hs@�9X@��m@��y@�v�@�-@���@��/@�@�
=@���@��T@�bN@�@�`B@���@�+@�~�@�E�@�@���@���@㕁@�$�@�$�@���@��@ٲ-@��@�ƨ@�$�@�r�@ӥ�@�V@�Z@ϥ�@�ȴ@�{@ͺ^@�X@�1'@��H@�?}@�  @��@�E�@š�@�/@ă@��@���@��@�A�@�V@��^@�/@���@��@�=q@�@��@��@��-@���@� �@�;d@��-@��7@��@�"�@���@��@���@�&�@��j@��@���@�@��@�Q�@�|�@�dZ@�\)@�\)@�b@��h@���@�v�@�dZ@�;d@�;d@�;d@�C�@�\)@�C�@��@���@��
@��@�  @�;d@�"�@��@���@��+@�E�@�=q@���@�r�@��@��@�o@��y@��!@�=q@�hs@���@��@�;d@�V@�n�@�v�@�n�@�^5@�-@�@��@���@���@�p�@�X@�O�@�G�@�G�@�?}@���@��9@�A�@��;@�;d@�@�l�@���@���@��;@�33@��!@�J@���@���@�@�x�@�7L@��@�bN@�b@��@�S�@�ff@�-@���@��^@�z�@��@��w@��P@�|�@�|�@�l�@�\)@�K�@�33@�"�@��!@�$�@�hs@�?}@��@�V@���@��u@�1@��;@��;@�K�@�"�@��+@��T@��@�r�@��!@�E�@�J@��-@�p�@�/@��@�%@���@��9@��@��D@�bN@�Q�@�1'@��@�  @��@�@~ff@~@~��@~{@}�@|j@{�@{�@{S�@z�H@z�!@z�@y��@yhs@yG�@y%@xQ�@x�9@xbN@x �@w;d@w
=@w�P@w�@w��@up�@t�@t(�@rn�@q�@r=q@r�!@s@so@sS�@s"�@sC�@s�@sS�@r�H@s�
@t�@sƨ@s@r�H@sC�@sS�@sS�@s"�@r�H@q�@p�@p�u@p��@p�9@p�9@p�u@pQ�@o�;@o+@n��@n��@m�@mO�@lz�@l�@k�m@kS�@j~�@jJ@i�#@iX@ihs@iX@i%@h��@h1'@g
=@e�T@e@e�-@e�-@e�-@e�@d(�@c33@b=q@b�@a�7@`��@`�u@`1'@`  @_�@`Q�@a&�@a%@`��@`Q�@_��@_|�@_l�@_K�@_
=@^�@^�R@^��@^�+@^v�@^v�@^v�@^ff@^5?@]@]?}@\�j@\j@\Z@\Z@\Z@\9X@\�@[t�@[33@Z�@Z��@Z��@Z~�@Z^5@Z=q@Z�@Yhs@YG�@Y&�@Y&�@Y�@Y%@X��@X��@XbN@Xb@W�@Wl�@W\)@V��@V��@V@Up�@T��@TI�@S�
@S��@S"�@R�H@R��@R�!@R^5@Q��@Qhs@Q�@P�`@PĜ@P�@PbN@PQ�@P1'@Pb@O�;@OK�@Nȴ@Nv�@N$�@M�@M�-@M?}@L��@L�D@L9X@K�m@KS�@J�@J��@Jn�@I�@I��@I&�@H�u@Hb@Gl�@G
=@Fȴ@F��@FV@E�h@E?}@EV@D��@D�@D��@D�j@D9X@C�m@C�m@C�m@C�m@C�
@C�F@CS�@B�!@A�@A��@A�7@A��@A�7@Ax�@Ax�@Ahs@AG�@@��@@��@@Ĝ@@�9@@A�@@b@?�@?l�@?K�@?�@>��@>�y@>E�@=@=p�@=V@<I�@;��@;��@;��@;��@;��@;�@;dZ@:�H@:^5@:M�@:�@9��@9�#@9�^@9��@9��@9x�@9G�@8��@8Ĝ@8��@8r�@81'@7��@7|�@7
=@5@5�@4��@4��@5�@5V@4j@4(�@4�@3��@3�F@3�@3o@2��@2��@2�\@2~�@2=q@1��@1��@0��@0�u@0r�@/��@/�P@/|�@/K�@/�@.��@.v�@.@.@-�-@-p�@-`B@-?}@-�@-�@,��@,�@,��@,��@,z�@,I�@,1@+��@+ƨ@+dZ@*��@*n�@*-@)��@)��@)7L@(��@(��@(��@(�9@(�u@(�@(�@(r�@(Q�@( �@(b@'�;@'��@'l�@'\)@';d@'+@'�@&�y@&��@&5?@%�@%��@%?}@$��@$�j@$z�@$(�@#ƨ@#��@#dZ@"�@"��@"n�@"M�@"-@!��@!hs@!&�@!%@ ��@ �9@ ��@ bN@ Q�@ b@��@�w@�@�@��@K�@+@
=@
=@ȴ@�+@v�@V@$�@�@��@�h@p�@`B@?}@�@�D@Z@(�@�
@�@33@�@��@�@�!@~�@n�@^5@=q@��@��@�7@&�@r�@�@��@�w@l�@+@�y@$�@�-@?}@�D@�@1@1@1@1@��@�F@33@�!@=q@J@�#@��@��@hs@7L@&�@�@��@�`@��@�u@b@�@\)@��@�R@v�@V@E�@5?@$�@$�@$�@{@��@p�@?}@�@�j@j@(�@1@�m@�F@��@t�@C�@
�@
�!@
~�@
^5@
�@	�#@	�^@	x�@	G�@�`@Ĝ@Ĝ@Ĝ@r�@A�@�@�@�P@l�@\)@;d@;d@�@�@��@ff@�T@�-@��@p�@?}@�@��@�/@��@z�@I�@�@��@��@1@�
@�
@ƨ@��@t�@S�@"�@�@��@�!@��@�\@n�@^5@M�@=q@-@�@J@�@�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBVBVBVBT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BS�BS�BR�BP�BL�BI�BF�BE�BC�B@�B>wBC�BI�BG�B>wB5?B,B�B\BDBB��B�B�)B��B�wB��B��B��B�PB�1B�+B�B� By�Bq�B`BBXBP�BC�B,B�BB�ZB�qB�B��B�uB�bB�VB�JB�1B{�Bw�BgmB`BBXBR�BM�B?}B6FB2-B)�B�BJB
��B
�B
�
B
ǮB
B
�dB
��B
��B
�JB
r�B
k�B
k�B
iyB
e`B
\)B
R�B
N�B
L�B
A�B
<jB
33B
+B
#�B
!�B
�B
bB
  B	�B	�HB	�
B	ɺB	��B	�^B	�-B	��B	��B	�hB	�=B	�1B	~�B	q�B	l�B	e`B	]/B	YB	T�B	Q�B	P�B	L�B	I�B	E�B	=qB	;dB	9XB	9XB	9XB	8RB	8RB	7LB	5?B	49B	33B	2-B	0!B	-B	&�B	 �B	�B	oB	VB	JB	1B	B	B	B��B��B��B��B��B�B�B�B�yB�fB�TB�HB�BB�5B�#B�
B�B��B��B��B��BɺBȴBƨBÖB��B��B��B�}B�qB�^B�LB�LB�FB�9B�!B�B��B��B��B��B��B��B��B��B��B�uB�hB�\B�DB�=B�+B�B�B~�B{�By�Bw�Bs�Bq�Bp�Bl�BjBaHB^5B]/B]/BYBW
BVBS�BQ�BM�BK�BI�BG�BC�BA�B?}B>wB:^B8RB7LB6FB6FB5?B49B33B/B-B'�B&�B&�B#�B#�B"�B"�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBoBhBbB\BJBDB
=B
=B
=B
=B
=B	7B1B	7B1B+B1B1B+B+B+B+B1B	7B	7B
=BDBDBDBJBVBVB\BbB\BbBhB{B�B�B{B�B�B �B �B#�B&�B/B>wBA�BE�BN�BO�BL�BK�BK�BK�BI�BH�BF�BC�BD�BG�BJ�BR�BcTBffBjBk�Bp�Bq�Br�Bt�Bv�Bw�By�B|�B�B�oB��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�RB�jB�}B��BÖBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�#B�#B�B�)B�/B�;B�NB�NB�ZB�ZB�TB�TB�ZB�fB�mB�mB�mB�sB�sB�sB�sB�mB�mB�fB�ZB�B�B�B��B��B��B��B��B��B��B��B��B	B	%B	1B	DB	JB	PB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	'�B	)�B	,B	/B	/B	/B	1'B	2-B	33B	5?B	6FB	8RB	:^B	<jB	=qB	A�B	F�B	E�B	D�B	D�B	C�B	C�B	D�B	D�B	D�B	E�B	E�B	F�B	F�B	J�B	R�B	XB	]/B	]/B	aHB	bNB	e`B	gmB	hsB	hsB	ffB	jB	p�B	r�B	v�B	~�B	� B	�B	�B	�B	�+B	�7B	�7B	�7B	�\B	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�9B	�3B	�?B	�RB	�XB	�^B	�^B	�^B	�dB	�dB	�dB	�jB	�wB	��B	B	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�)B	�/B	�5B	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
DB
JB
JB
PB
PB
VB
\B
\B
bB
bB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
1'B
2-B
33B
49B
33B
33B
33B
49B
5?B
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
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
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
K�B
L�B
L�B
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
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
R�B
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
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
ffB
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
hsB
hsB
hsB
hsB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
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
p�B
p�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBVBV9BVBT�BT�BT�BT�BUBUBT�BT�BT�BUBTBTBS&BQNBMBJ#BGEBF�BF%BEBG_BO\BO�BL�BEB:�B2�B#B B�B	�B��B��B��B�TB��B�0B��B�B�<B��B��B�YB��B|PBt�Ba�BY�BT�BG�B0;BxB	B�B��B�B�B�B��B�B�B��B}�BzxBh�Ba|BYBT{BO\B@iB7fB3�B,B7BVB
��B
�B
خB
ȴB
�3B
�B
�0B
��B
��B
t9B
lqB
l�B
k6B
g�B
\�B
S�B
O�B
NB
B�B
=�B
4�B
+�B
$@B
"�B
�B
�B
[B	�5B	��B	خB	�)B	��B	��B	�B	��B	��B	�&B	�B	�XB	��B	r�B	m�B	f�B	^5B	ZB	U�B	RoB	Q�B	M�B	K)B	GzB	>B	<PB	9�B	9�B	9�B	9>B	9�B	88B	5�B	4�B	3�B	3B	1AB	/B	)B	"�B	7B	�B	�B	�B		7B	�B	MB	-B��B��B��B��B�zB�TB��B�B��B�mB��B��B�bB��B�]B��B��B�B�B�B�B�XB�lBǮB�MB�B�UB��B��B�.B�B��B��B�2B��B��B�=B�
B��B��B�\B�VB��B��B��B�sB�{B��B�bB�0B�)B�1B�tB�B� B|�Bz�Bx�BtTBr�Br-Bn�Bl�BbB^�B^B^5BYBW�BW$BVBSuBN�BL�BK�BIBD�BBuBA B?�B;JB8�B7�B6�B6�B5�B5�B5B1�B.cB(�B(
B'�B$tB$@B#�B#�B!-B!|B 'B 'BVB�B�B/B�BxB�B�B�BBBB�B�B�B@BoB�B�B"B�B
�B
�BDB^B^B
#B	�B	�B�B�B�B�B�B�B1BKB	B	�B	�B
�B�B�B�B�B(B�B}B B�BB�B2BBBB�BxB!|B!�B$�B'B.}B>wBAoBF%BO�BP�BMPBL~BL�BL�BJ�BIlBG+BC�BD�BG�BJ�BR�Bd@BhXBl"Bk�Bp�Bq�Br�Bt�BwBxRBz�B~wB��B��B�7B��B��B��B�!B��B� B�TB��B��B�B�)B�vB��B��B�"B� B�'B�gB�XB��B��B��B�B�"B�B��B�B�(B�.B��B�B� B�B�B�SB�kBیBیBۦB�7B��B�B�;B�B��B��B��B�B�nB�B��B�B�B��B��B��B��B�*B�B�B��B�,B��B��B��B��B��B��B��B��B�B�$B�XB�]B	�B	YB	fB	xB	�B	�B	�B	�B	�B	B	�B	=B	)B	CB	WB	�B	)B	�B	# B	%B	&2B	($B	*0B	,=B	/5B	/OB	/OB	1[B	2aB	3MB	5tB	6zB	8�B	:�B	<�B	=�B	A�B	F�B	E�B	EB	EB	C�B	C�B	D�B	D�B	D�B	E�B	E�B	F�B	F�B	KB	R�B	X_B	]dB	]~B	aHB	bNB	e`B	g�B	iDB	h�B	f�B	kB	p�B	r�B	v�B	~�B	�B	�B	�-B	�B	�+B	�lB	�RB	�B	�\B	��B	��B	��B	��B	��B	��B	��B	�2B	�sB	�WB	�"B	�/B	�5B	�5B	�OB	�;B	�UB	�aB	�TB	�hB	��B	��B	��B	��B	�xB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�)B	�B	��B	��B	�B	�B	�pB	�.B	�HB	��B	�B	�4B	�B	�&B	�,B	�B	��B	��B	�WB	�xB	�dB	ބB	�|B	�hB	�B	�B	�nB	�B	�zB	�`B	�B	�B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	��B	�B	�B	�B	�6B	�<B	�<B	�(B	�.B
 4B
 4B
 B
UB
AB
GB
-B
GB
gB
9B
YB
_B
_B
fB
KB
	7B
	RB
	lB
	lB
	�B
�B
dB
~B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#B
#B
$B
$B
%B
$�B
%B
%�B
%�B
&2B
'8B
'8B
(>B
(XB
)*B
)�B
*B
*B
)�B
*0B
*B
*0B
+QB
+6B
,=B
,=B
,=B
,"B
,B
,=B
,"B
,"B
-CB
-)B
-)B
-)B
-)B
-CB
.IB
.IB
.}B
/5B
/iB
0;B
1AB
2GB
3�B
4nB
3hB
3MB
3MB
4nB
5tB
6`B
7LB
7�B
7�B
7fB
8�B
8�B
8�B
9rB
:xB
:�B
:�B
:xB
:xB
:�B
;�B
;�B
;B
;B
;B
<�B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
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
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
K�B
MB
L�B
L�B
M�B
M�B
M�B
NB
M�B
N�B
N�B
N�B
OB
N�B
O�B
PB
O�B
PB
O�B
Q B
Q B
QB
QB
QB
QB
RB
R B
RB
RB
S&B
T,B
T,B
T,B
TB
TB
T,B
SB
R�B
UB
U2B
UB
UB
U2B
UB
U2B
UB
U2B
UMB
U2B
U2B
UB
U2B
VB
VSB
VSB
WYB
W?B
W?B
XEB
X+B
XB
XB
XB
XEB
X+B
YeB
YKB
ZkB
[=B
[=B
[=B
[=B
\]B
\]B
\CB
\)B
\]B
\CB
\]B
\CB
]~B
]dB
^OB
^jB
^jB
^jB
_;B
_;B
_VB
_;B
_;B
_;B
_pB
_pB
`\B
`\B
`\B
`\B
`\B
a|B
abB
a|B
abB
abB
bhB
bhB
bhB
c�B
cnB
dtB
dtB
dtB
e�B
ezB
e�B
f�B
f�B
g�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
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
p�B
p�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<j<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609130034072016091300340720160913003407201806221213412018062212134120180622121341201804050406182018040504061820180405040618  JA  ARFMdecpA19c                                                                20160909093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160909003529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160909003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160909003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160909003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160909003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160909003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160909003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160909003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160909003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20160909011857                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160909153528  CV  JULD            G�O�G�O�F�K�                JM  ARGQJMQC2.0                                                                 20160909153528  CV  JULD_LOCATION   G�O�G�O�F�K�                JM  ARGQJMQC2.0                                                                 20160909153528  CV  LONGITUDE       G�O�G�O��#O                JM  ARCAJMQC2.0                                                                 20160912153407  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160912153407  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190618  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031341  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                