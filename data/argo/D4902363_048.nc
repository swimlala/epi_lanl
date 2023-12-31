CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-18T00:35:32Z creation;2016-10-18T00:35:34Z conversion to V3.1;2019-12-19T08:27:40Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20161018003532  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  I2_0576_048                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��5�� �1   @��6m�5 @:��S����d�Ƨ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
=@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B��
B��
B���B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD���D��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD�
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A�ȴAԺ^A԰!AԬAԬAԬAԬAԩ�Aԩ�Aԧ�Aԡ�A�z�A�(�Aӧ�A�G�A�O�A�;dA�JA˩�A��/A�hsA²-A���A�7LA���A�ZA���A���A��uA��A��A���A�ZA��#A��PA�33A�ĜA�ffA���A���A�ĜA���A�JA�ffA�I�A��-A���A���A�A�G�A�1'A�z�A�+A��A�{A��jA��A�"�A��A�5?A��A�p�A��A�VA�n�A��A��;A�ƨA�;dA�ĜA�|�A�n�A�S�A�$�A�-A~JA|�`A{
=Ay�hAw��Avr�Au�FAu�At��As�hAr  Ao�An�`An=qAm�Al�yAl(�AkAh��Ag�Af5?Ae��Ae`BAc�Ab��Aa�FAa+A`��A_��A]��AZ�+AZZAZ{AY�PAY"�AX1AW"�AVVAV1AUAU33AT��AT  AS&�AR�RAQ�^AO��AN{AM`BAM%AL��AL�uALr�AL^5AL$�AJ �AF�`AE��AD�ADA�AC�;ACdZAB�ABĜAB��ABM�AA�mAAp�A@5?A>��A<�!A9��A8=qA7l�A6�RA6bA5\)A4��A41'A3��A3x�A2�`A2$�A1�A0�+A0$�A/��A/�PA/&�A.=qA-/A+��A*�9A*�uA*bNA)C�A(jA(A'�A&ZA%��A%t�A#�hA#?}A"n�A!hsA ��A =qA�A��A��AȴA5?AƨAl�A�9A��Ap�A�A;dAE�AK�A9XA��A�#A��An�A$�A"�A��A��A�uAI�A�PA
ffA	;dAZA�mA�HA��AC�A�/A�mA�jAC�A Q�@�ȴ@�ff@��7@���@�
=@�-@���@��j@�o@�  @�^5@�`B@��@��@�G�@�F@�n�@���@�  @�5?@�+@�!@�7@�1@ߕ�@�l�@�"�@��`@�+@�ȴ@ڗ�@�5?@ٺ^@�Q�@և+@�/@��H@с@�Ĝ@�9X@�l�@�
=@�ff@���@́@�I�@�V@ɩ�@�X@�%@���@�9X@Ǿw@���@�-@Ų-@�p�@��@��/@ģ�@�j@�A�@�C�@���@���@��@���@���@��D@���@�v�@�J@���@���@�J@�v�@��@���@�V@��#@�`B@�r�@���@��@���@���@�r�@�b@���@��y@�^5@���@�r�@�"�@���@�M�@��^@��^@��/@�dZ@�?}@�Ĝ@���@�Ĝ@�A�@��@��/@�I�@�bN@� �@��w@�"�@��@���@�Q�@��;@�b@�l�@�
=@�@��#@�%@�1@��F@�dZ@�S�@�K�@�@���@�v�@�@�(�@�ƨ@�o@�{@��@��`@�9X@�S�@��R@�E�@��@��@���@��/@��j@���@�9X@��@�|�@�S�@�33@��y@�E�@���@��@���@���@���@�bN@�9X@��;@���@��@�|�@�
=@�ff@�M�@�=q@�$�@��T@��-@�`B@��@��`@��@�j@�bN@�9X@�  @��
@��w@�K�@��@�~�@�E�@�5?@��T@��#@���@��-@���@���@�x�@��@�z�@���@���@���@��@�\)@�;d@��!@�ff@�^5@�=q@��T@���@��@�O�@�/@��@���@��/@���@���@�Z@�(�@�1@\)@
=@~�R@~��@~E�@}@}/@}V@|z�@{�m@{��@{t�@z�!@y�#@yx�@yhs@yG�@y7L@y�@x�u@w�@w�P@wK�@v�y@vff@u��@u/@t�/@tZ@t1@s�F@sC�@rn�@r-@r�@q��@qhs@qX@q��@q��@q��@qx�@qX@q�@q7L@q�^@qG�@p �@o��@o�@o+@n�y@n�@o+@ol�@ol�@o;d@n��@n��@o
=@n��@nȴ@n��@nE�@m�@m��@m�@mO�@m/@mV@l��@l�@l(�@kS�@k@j��@j^5@i��@i�7@i&�@h�@g�P@g�@f�R@fE�@f5?@f5?@e@e�@d�D@dj@dI�@d�@d1@d1@c�m@c��@a��@a7L@`�`@`��@`Ĝ@`r�@`Q�@_�;@_�P@_
=@^v�@^{@]�T@]@]@]�-@]��@]@\��@ZM�@Z-@ZM�@ZM�@Y��@Y�@Y��@Y��@Y�7@Y7L@Y&�@Y&�@X��@X��@X1'@W�P@W�@Vv�@U�h@UO�@T�@U�@U�-@U�@U/@T�j@T�j@T�j@S�
@SC�@S33@S"�@So@S@R�@R�@R�H@R�H@R��@R��@Rn�@R�@Q�@Q��@Qx�@QG�@Q%@Q%@Q%@P��@PĜ@PĜ@P�u@PbN@PA�@O�@O�@O|�@OK�@O�@N�y@N�R@N$�@M��@M`B@L��@Lz�@LI�@L�@K��@K�F@KdZ@Ko@J�@J�H@J��@J~�@J-@I�7@I%@H�9@HA�@Hb@Hb@Hb@G�;@G�w@G|�@F��@F��@F��@F��@F�+@Fv�@F{@E�@Ep�@E/@EV@D��@D��@D�/@D�/@D�j@D�@D��@DI�@C�
@Co@B�\@B-@A�^@A�7@Ahs@AG�@A7L@A&�@A%@@��@@�`@@�`@@��@@bN@@b@?�@?\)@?K�@>��@>�y@>ȴ@>�R@>v�@>$�@>{@>{@=�@=�T@=�T@=�T@=�h@<�/@<j@<(�@<1@;�m@;�F@;dZ@;C�@:�@:M�@:=q@9�#@9��@9&�@8��@8Ĝ@8�u@81'@8b@7�@7l�@7l�@7l�@7\)@7+@6�@6E�@5�-@5�@5`B@5?}@4��@4�@41@41@3��@333@3o@2��@2M�@1�#@1hs@1�@1%@0�`@0�9@0b@/�@/�@/��@/;d@.�+@.E�@.@-�h@-V@,�/@,�D@,z�@,Z@,I�@,(�@+��@+ƨ@+t�@+"�@+o@*��@*J@)�#@)�@(��@(�`@(�`@(r�@'�P@'\)@'+@'
=@&�y@&��@&v�@%�T@%�-@%�h@%�@%p�@%/@$�j@$�D@$Z@#�m@#��@"�H@"~�@!�@!X@!%@ ��@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ �9@ ��@ �u@ �@ A�@��@��@|�@K�@
=@�R@��@�+@ff@ff@V@�@�-@p�@`B@`B@/@�/@z�@I�@�@�
@�F@��@t�@C�@"�@@��@��@��@M�@=q@-@-@-@-@-@-@�@�#@��@��@�7@X@G�@7L@%@��@Q�@  @��@�w@�w@�w@�w@�w@��@l�@;d@��@�R@ff@V@5?@5?@@�@p�@`B@`B@O�@/@��@�/@�@9X@ƨ@��@�@t�@S�@"�@�H@�!@M�@��@�^@��@G�@�`@�9@Q�@b@�@��@�@|�@\)@;d@�@
=@�y@�@ȴ@�R@�R@�R@ȴ@�R@��@�+@E�@$�@@��@@�-@�h@p�@��@��@�D@z�@z�@�D@z�@I�@��@�@S�@C�@o@
�@
�H@
��@
��@
~�@
n�@
^5@
M�@
=q@	��@	�@	�#@	�7@	G�@	&�@	�@��@�9@�@bN@1'@�@��@�P@\)@�@K�@+@�y@�@�R@�+@v�@ff@V@E�@E�@$�@�T@�@O�@�j@�j@�@��@�D@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A�ȴAԺ^A԰!AԬAԬAԬAԬAԩ�Aԩ�Aԧ�Aԡ�A�z�A�(�Aӧ�A�G�A�O�A�;dA�JA˩�A��/A�hsA²-A���A�7LA���A�ZA���A���A��uA��A��A���A�ZA��#A��PA�33A�ĜA�ffA���A���A�ĜA���A�JA�ffA�I�A��-A���A���A�A�G�A�1'A�z�A�+A��A�{A��jA��A�"�A��A�5?A��A�p�A��A�VA�n�A��A��;A�ƨA�;dA�ĜA�|�A�n�A�S�A�$�A�-A~JA|�`A{
=Ay�hAw��Avr�Au�FAu�At��As�hAr  Ao�An�`An=qAm�Al�yAl(�AkAh��Ag�Af5?Ae��Ae`BAc�Ab��Aa�FAa+A`��A_��A]��AZ�+AZZAZ{AY�PAY"�AX1AW"�AVVAV1AUAU33AT��AT  AS&�AR�RAQ�^AO��AN{AM`BAM%AL��AL�uALr�AL^5AL$�AJ �AF�`AE��AD�ADA�AC�;ACdZAB�ABĜAB��ABM�AA�mAAp�A@5?A>��A<�!A9��A8=qA7l�A6�RA6bA5\)A4��A41'A3��A3x�A2�`A2$�A1�A0�+A0$�A/��A/�PA/&�A.=qA-/A+��A*�9A*�uA*bNA)C�A(jA(A'�A&ZA%��A%t�A#�hA#?}A"n�A!hsA ��A =qA�A��A��AȴA5?AƨAl�A�9A��Ap�A�A;dAE�AK�A9XA��A�#A��An�A$�A"�A��A��A�uAI�A�PA
ffA	;dAZA�mA�HA��AC�A�/A�mA�jAC�A Q�@�ȴ@�ff@��7@���@�
=@�-@���@��j@�o@�  @�^5@�`B@��@��@�G�@�F@�n�@���@�  @�5?@�+@�!@�7@�1@ߕ�@�l�@�"�@��`@�+@�ȴ@ڗ�@�5?@ٺ^@�Q�@և+@�/@��H@с@�Ĝ@�9X@�l�@�
=@�ff@���@́@�I�@�V@ɩ�@�X@�%@���@�9X@Ǿw@���@�-@Ų-@�p�@��@��/@ģ�@�j@�A�@�C�@���@���@��@���@���@��D@���@�v�@�J@���@���@�J@�v�@��@���@�V@��#@�`B@�r�@���@��@���@���@�r�@�b@���@��y@�^5@���@�r�@�"�@���@�M�@��^@��^@��/@�dZ@�?}@�Ĝ@���@�Ĝ@�A�@��@��/@�I�@�bN@� �@��w@�"�@��@���@�Q�@��;@�b@�l�@�
=@�@��#@�%@�1@��F@�dZ@�S�@�K�@�@���@�v�@�@�(�@�ƨ@�o@�{@��@��`@�9X@�S�@��R@�E�@��@��@���@��/@��j@���@�9X@��@�|�@�S�@�33@��y@�E�@���@��@���@���@���@�bN@�9X@��;@���@��@�|�@�
=@�ff@�M�@�=q@�$�@��T@��-@�`B@��@��`@��@�j@�bN@�9X@�  @��
@��w@�K�@��@�~�@�E�@�5?@��T@��#@���@��-@���@���@�x�@��@�z�@���@���@���@��@�\)@�;d@��!@�ff@�^5@�=q@��T@���@��@�O�@�/@��@���@��/@���@���@�Z@�(�@�1@\)@
=@~�R@~��@~E�@}@}/@}V@|z�@{�m@{��@{t�@z�!@y�#@yx�@yhs@yG�@y7L@y�@x�u@w�@w�P@wK�@v�y@vff@u��@u/@t�/@tZ@t1@s�F@sC�@rn�@r-@r�@q��@qhs@qX@q��@q��@q��@qx�@qX@q�@q7L@q�^@qG�@p �@o��@o�@o+@n�y@n�@o+@ol�@ol�@o;d@n��@n��@o
=@n��@nȴ@n��@nE�@m�@m��@m�@mO�@m/@mV@l��@l�@l(�@kS�@k@j��@j^5@i��@i�7@i&�@h�@g�P@g�@f�R@fE�@f5?@f5?@e@e�@d�D@dj@dI�@d�@d1@d1@c�m@c��@a��@a7L@`�`@`��@`Ĝ@`r�@`Q�@_�;@_�P@_
=@^v�@^{@]�T@]@]@]�-@]��@]@\��@ZM�@Z-@ZM�@ZM�@Y��@Y�@Y��@Y��@Y�7@Y7L@Y&�@Y&�@X��@X��@X1'@W�P@W�@Vv�@U�h@UO�@T�@U�@U�-@U�@U/@T�j@T�j@T�j@S�
@SC�@S33@S"�@So@S@R�@R�@R�H@R�H@R��@R��@Rn�@R�@Q�@Q��@Qx�@QG�@Q%@Q%@Q%@P��@PĜ@PĜ@P�u@PbN@PA�@O�@O�@O|�@OK�@O�@N�y@N�R@N$�@M��@M`B@L��@Lz�@LI�@L�@K��@K�F@KdZ@Ko@J�@J�H@J��@J~�@J-@I�7@I%@H�9@HA�@Hb@Hb@Hb@G�;@G�w@G|�@F��@F��@F��@F��@F�+@Fv�@F{@E�@Ep�@E/@EV@D��@D��@D�/@D�/@D�j@D�@D��@DI�@C�
@Co@B�\@B-@A�^@A�7@Ahs@AG�@A7L@A&�@A%@@��@@�`@@�`@@��@@bN@@b@?�@?\)@?K�@>��@>�y@>ȴ@>�R@>v�@>$�@>{@>{@=�@=�T@=�T@=�T@=�h@<�/@<j@<(�@<1@;�m@;�F@;dZ@;C�@:�@:M�@:=q@9�#@9��@9&�@8��@8Ĝ@8�u@81'@8b@7�@7l�@7l�@7l�@7\)@7+@6�@6E�@5�-@5�@5`B@5?}@4��@4�@41@41@3��@333@3o@2��@2M�@1�#@1hs@1�@1%@0�`@0�9@0b@/�@/�@/��@/;d@.�+@.E�@.@-�h@-V@,�/@,�D@,z�@,Z@,I�@,(�@+��@+ƨ@+t�@+"�@+o@*��@*J@)�#@)�@(��@(�`@(�`@(r�@'�P@'\)@'+@'
=@&�y@&��@&v�@%�T@%�-@%�h@%�@%p�@%/@$�j@$�D@$Z@#�m@#��@"�H@"~�@!�@!X@!%@ ��@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ �9@ ��@ �u@ �@ A�@��@��@|�@K�@
=@�R@��@�+@ff@ff@V@�@�-@p�@`B@`B@/@�/@z�@I�@�@�
@�F@��@t�@C�@"�@@��@��@��@M�@=q@-@-@-@-@-@-@�@�#@��@��@�7@X@G�@7L@%@��@Q�@  @��@�w@�w@�w@�w@�w@��@l�@;d@��@�R@ff@V@5?@5?@@�@p�@`B@`B@O�@/@��@�/@�@9X@ƨ@��@�@t�@S�@"�@�H@�!@M�@��@�^@��@G�@�`@�9@Q�@b@�@��@�@|�@\)@;d@�@
=@�y@�@ȴ@�R@�R@�R@ȴ@�R@��@�+@E�@$�@@��@@�-@�h@p�@��@��@�D@z�@z�@�D@z�@I�@��@�@S�@C�@o@
�@
�H@
��@
��@
~�@
n�@
^5@
M�@
=q@	��@	�@	�#@	�7@	G�@	&�@	�@��@�9@�@bN@1'@�@��@�P@\)@�@K�@+@�y@�@�R@�+@v�@ff@V@E�@E�@$�@�T@�@O�@�j@�j@�@��@�D@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bx�B~�B�JB��B��B�uB~�BiyB^5B)�B�B	7B�B�TB�
BȴBB�?B��B��B��B��B�!B��B��B�hB�%BjB>wBP�B\)Bl�Bw�Bq�Bp�BiyBZB^5BW
BH�B=qB,BbB��B�mBǮB�B��B�JB\)BD�B33B49BJB
��B
�B
�B
��B
B
�qB
�RB
�LB
�FB
�3B
��B
��B
�oB
�7B
~�B
v�B
m�B
hsB
dZB
aHB
YB
P�B
B�B
:^B
49B
0!B
+B
$�B
�B
	7B
B	�B	�B	�B	�B	�;B	�B	��B	��B	ɺB	�qB	��B	��B	��B	�{B	�hB	�DB	�%B	�%B	�1B	�%B	�B	{�B	v�B	q�B	t�B	w�B	k�B	aHB	[#B	XB	VB	Q�B	P�B	N�B	K�B	A�B	+B	�B	�B	�B	{B	oB	VB	PB	DB	
=B	+B	B��B�B�mB�B��B��B��B��BȴBǮBƨBŢBĜBĜBB��B��B�wB�wB�jB�qB�dB�^B�XB�?B�3B�-B�'B�B�B�B��B��B��B��B��B��B��B�hB�bB�PB�1B�%B�B� B~�B|�B|�By�Bx�Bt�Bq�Bm�Bk�BffBdZBbNB[#B[#BYBXBT�BS�BO�BM�BL�BG�BE�BC�BB�B@�B<jB:^B8RB8RB33B1'B/B.B.B.B-B-B-B,B+B+B&�B%�B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B �B�B�B�B�B�B�B)�B(�B)�B+B-B/B1'B9XB>wB?}B?}B@�BA�BB�BE�BE�BD�BE�BE�BE�BG�BF�BH�BH�BE�BJ�BM�BL�BL�BO�BL�BJ�BL�BW
BZB\)B^5B^5B]/B_;B`BBaHBbNBe`Bl�Bp�Bn�Br�Bu�By�B{�B~�B� B�B�%B�7B�=B�DB�PB�VB�\B�hB��B��B��B��B��B��B��B��B�B�B�-B�9B�?B�FB�FB�LB�^B�qB�wB�}B��B��BŢBƨBǮBɺB��B��B��B��B�B�
B�B�B�)B�HB�HB�NB�NB�ZB�`B�sB�B�B�B��B��B��B��B��B��B��B	B	%B	1B	1B		7B		7B	DB	JB	JB	PB	PB	bB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	)�B	,B	-B	/B	1'B	2-B	33B	33B	49B	8RB	<jB	=qB	=qB	@�B	B�B	D�B	D�B	F�B	H�B	K�B	K�B	M�B	O�B	P�B	P�B	T�B	YB	]/B	^5B	^5B	_;B	_;B	aHB	cTB	dZB	e`B	ffB	jB	l�B	o�B	q�B	s�B	t�B	v�B	x�B	� B	�B	�B	�B	�B	�1B	�PB	�VB	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�?B	�?B	�?B	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�jB	�qB	��B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�5B	�BB	�TB	�ZB	�NB	�HB	�NB	�ZB	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
%B
%B
+B
1B
1B
	7B

=B

=B

=B
DB
DB
DB
JB
PB
VB
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
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
/B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
33B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
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
L�B
L�B
M�B
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
N�B
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
R�B
R�B
R�B
R�B
R�B
R�B
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
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
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
YB
YB
YB
YB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
^5B
]/B
^5B
^5B
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
bNB
bNB
bNB
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
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
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
n�B
n�B
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
q�B
p�B
q�B
p�B
p�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�BxByrB�B��B�)B��B�EB�gBp;Bh$B.BB�B��B�>B��B�DB��B�B�wB��B��B��B�9B��B��B�&B��Bn}B?�BQ B]IBo�Bx�Br�Br�BmB\�B`�BZ�BK�BA�B0�BB[B�=B�B��B��B��B_�BG�B6B9�BVB�B
�B
ۦB
�B
ÖB
�B
��B
��B
��B
�B
��B
�QB
��B
�DB
� B
xRB
n�B
iDB
e`B
b�B
[#B
SB
C�B
;JB
5B
1'B
,=B
&�B
�B

�B
�B	�B	��B	�MB	�B	�vB	��B	��B	�bB	̳B	�iB	��B	�#B	�KB	�MB	��B	�dB	�B	��B	��B	��B	��B	|�B	w�B	r�B	v`B	z*B	m)B	bB	[�B	XyB	VmB	R:B	QNB	O�B	N�B	D�B	,�B	 �B	]B	EB	B	�B	�B	�B	�B	
�B	1B	�B�]B�B�B��B� B��B��BˬB�lBȴB�EB�?BňBżBÖB��B�;B��B��B�<B��B��B�B��B��B��B��B�-B��B� B�cB��B�$B��B��B��B�B��B�oB� B��B��B�EB��B��B�B~(B~(Bz�Bz�Bu�BsBoBmBg�Bf�Bc�B[�B[�BZkBYBVmBUgBP�BO(BN�BIRBF�BD�BDBA�B=<B;JB9�B:B5?B2�B0UB.�B.�B/5B-�B-�B-�B,�B,qB-B(
B&�B&B$�B"�B!�B �B�B�B�BdBWBB�BB�BeBB�BB�BB1B�B�B�B!HB �BOB/BOBBCBIB)B�B�B5B�B�BBIB5BjB \B;B B B 'B B!B BB �B"4B vB B�BBB�B*�B)_B*KB+B,�B/ B0�B9�B>�B@ B@BAoBBuBC�BFtBE�BD�BFBF%BF?BHKBG_BI�BI�BE�BK^BN<BMBM�BQBNBKDBL�BWYBZ�B]IB_�B^�B]IB_�B`�Ba�Bc Be�Bm]Bp�Bn�Bs3BvFBzxB|PB�B��B�uB�tB�RB��B��B��B��B�.B�oB��B�?B�_B�#B�OB��B��B��B��B��B�|B�nB�ZB�zB�zB��B��B��B��B��B��B�B��B��B��B�#B�B�BB� B�@B�SB�?B�EB�yBܒB�bB�|B�B�B�B��B�B�B��B��B��B��B��B�*B�0B�dB�HB	{B	tB	fB	�B		lB		lB	xB	dB	dB	�B	�B	�B	�B	�B	�B	�B	�B	�B	;B	!�B	#�B	%,B	'8B	*0B	,"B	-CB	/5B	1AB	2GB	3MB	3MB	4nB	8�B	<�B	=�B	=�B	@�B	B�B	D�B	D�B	F�B	IB	K�B	L0B	N"B	O�B	Q4B	QNB	UgB	YeB	]IB	^jB	^jB	_VB	_�B	a|B	c�B	d�B	e�B	f�B	j�B	l�B	o�B	q�B	s�B	t�B	wB	y>B	�4B	�;B	�AB	�[B	�B	�1B	�PB	�pB	��B	�}B	��B	��B	��B	��B	�#B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�"B	�)B	�/B	�5B	�;B	�AB	�GB	�hB	�TB	�tB	�ZB	�ZB	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ĶB	��B	��B	�B	��B	��B	��B	�B	��B	��B	�"B	�}B	�,B	�B	�B	�B	�2B	�9B	�?B	�EB	�_B	�kB	�WB	�]B	�CB	�CB	�CB	�OB	�vB	�B	��B	�hB	�bB	�hB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	�2B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�"B	�B	�B	�(B	�B	�.B
 4B
 4B
 4B
B
B
GB
9B
?B
?B
_B
KB
fB
	lB

XB

XB

rB
^B
^B
xB
�B
�B
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
�B
 B
!B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
"�B
#B
$B
#�B
$�B
%B
$�B
$�B
%�B
%�B
%�B
%�B
&B
&�B
'B
'B
&B
'8B
($B
)B
)*B
)B
)B
)B
*B
*0B
+QB
+B
+6B
,"B
,=B
-CB
-CB
-CB
./B
./B
.IB
/OB
/B
/B
/OB
/5B
0oB
0UB
1[B
2aB
2aB
2aB
2aB
3hB
3hB
4nB
3�B
4TB
5ZB
5tB
5tB
6�B
6�B
7fB
7�B
7fB
7�B
8�B
8lB
9rB
9rB
9�B
9�B
:xB
;�B
;�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
IB
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
KB
K�B
K�B
K�B
L�B
MB
M�B
M�B
NB
M�B
NB
M�B
N�B
N�B
N�B
N�B
OB
OB
O�B
O�B
O�B
O�B
QB
QB
Q B
Q B
Q B
QB
R B
R B
RB
RB
SB
R�B
SB
R�B
R�B
R�B
SB
S&B
SB
SB
SB
S&B
S&B
TB
TB
T,B
TFB
U2B
UB
U2B
VB
VB
VB
VB
VB
VB
VB
VB
W$B
W$B
X+B
X+B
X+B
X+B
XEB
X_B
YB
YB
YB
Y1B
Y1B
YKB
YKB
Y1B
ZkB
[=B
[=B
[WB
[WB
\CB
\]B
]IB
]IB
]dB
^OB
]dB
^OB
^jB
_pB
_pB
_pB
_VB
`vB
`vB
`\B
`\B
`\B
abB
a|B
aHB
bhB
bhB
bNB
cnB
cTB
cTB
cTB
cTB
cTB
cnB
c�B
dtB
d�B
dtB
dtB
ezB
ezB
ezB
e�B
f�B
f�B
ffB
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
jB
j�B
j�B
k�B
k�B
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
n�B
n�B
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
q�B
p�B
q�B
p�B
p�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610220037562016102200375620161022003756201806221215312018062212153120180622121531201804050408222018040504082220180405040822  JA  ARFMdecpA19c                                                                20161018093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161018003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161018003533  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161018003533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161018003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161018003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161018003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161018003534  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20161018003534                      G�O�G�O�G�O�                JA  ARUP                                                                        20161018012426                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161018153919  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20161018153919  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161021153756  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161021153756  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190822  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031531  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                