CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-08T00:35:20Z creation;2016-07-08T00:35:22Z conversion to V3.1;2019-12-19T08:36:14Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160708003520  20200115101516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_014                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׹�ъn 1   @׹���� @<b��}V�dxۋ�q1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|y�D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.{@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/G�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�BG�B��
B��
B��
B��
B��
B��
B��
B��
B�
=B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*t{D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|t{D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��=D�:=D�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�J=D�]q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AÇ+AÇ+AÉ7AÍPAÏ\AËDAÉ7A�~�A�z�A�x�A�n�A�^5A�I�A�9XA��/A§�A�%A�&�A��A�jA�I�A��A�K�A�ZA��jA�&�A���A�l�A�33A�Q�A�1'A�|�A�
=A���A�33A�33A�~�A�1A�|�A�ƨA�+A��A�x�A�%A�VA��A�+A�|�A���A��A���A�$�A���A�K�A�VA�A�5?A�%A�~�A�JA�bA��+A�`BA�{A��wA���A��DA�M�A���A� �A��A���A�A��\A�33A~�+A}l�A{�PAz{Ay|�AyAxr�Aw�7Av�\Av  Au%At��AsƨAp�ApZAn�An$�AmƨAm�AlE�Ak"�AjAhffAf��Af�Ae�TAe&�Ac��Ab�Ab�A`�A_O�A\�A\9XAZ�yAZ  AY|�AX�uAWƨAWG�AV��AU�-AT��AT�+ATM�AS�mASt�AS�AR-AQ33AP�/AP�9APn�AO�AN�`AN(�AM��AM`BALn�AJ��AI��AIAG�AEAEAC��AC33AB$�AA��A@��A@bNA??}A>bA=�^A=t�A<�!A<bNA;ƨA:�HA:JA9�A9�A81'A7x�A6ȴA6ffA57LA4v�A41A4  A3��A3��A3;dA2�RA1��A0�A0bA/O�A-oA+�7A*�9A)�A)+A)oA)A(��A'�7A&��A&I�A%�hA$5?A"�A!�;A (�AG�A�AI�A�AoA-A��A"�AE�AoA��A��A~�AI�A��AS�Az�A�-A�+A  AXA��AA�A��A
��A	K�A �At�A��AVA�A�7A��A�HAȴAhsA��A�+A�@�$�@��u@�bN@�(�@�l�@��H@�/@� �@�ƨ@��@��@�D@�!@�$�@��m@�@�M�@睲@���@�x�@�I�@�t�@�M�@�z�@߾w@��@�=q@���@۾w@��@�%@�;d@��@��#@ԣ�@ҸR@��@���@У�@Ѓ@��
@͡�@̴9@�C�@��@ʧ�@�ff@��@���@�C�@�
=@�@�@���@ƸR@�n�@ļj@�|�@��@�v�@��^@���@�dZ@�V@��^@��@��F@�C�@���@�A�@��@�ff@�X@�1@�K�@��R@�{@�x�@��u@�j@�1@�\)@�@�ff@���@�O�@�Ĝ@��D@��@�dZ@��H@��R@���@���@��@�9X@��;@�t�@�;d@�
=@�v�@��#@�/@�Z@�\)@�ȴ@�E�@�@���@�`B@�%@�z�@��@��w@�o@��\@�=q@��@�`B@���@��@�9X@�l�@�K�@�C�@��y@�-@���@��7@�1'@��;@���@�dZ@���@���@���@�V@�=q@�{@�?}@���@��@�b@��F@�t�@�ȴ@�J@�`B@�X@�/@��@�bN@�1@���@�C�@��R@���@�$�@��@��-@�hs@��@��@��m@���@�ƨ@��w@��@�\)@��!@�^5@�5?@�p�@��@���@�Q�@�  @�S�@�E�@�$�@�{@�@���@��@��@��T@���@��^@��@�&�@��j@���@��D@��m@��@�|�@�l�@�\)@�33@��!@��+@�ff@�@�X@���@���@��D@�9X@�  @�@�;@l�@;d@+@
=@
=@
=@~��@~V@}�-@}�@}`B@}�@|�/@|1@{o@z~�@zJ@yX@x�`@xr�@xA�@w�w@wK�@w+@w+@w�@v��@v�@v�R@v��@vv�@vE�@v$�@u@uO�@t�j@tZ@t�@s��@s@rn�@q�#@q��@q��@q�^@q��@qhs@q7L@p��@p�u@o�;@o\)@o�@nȴ@n�+@nV@nE�@m�T@m�@m?}@l�/@k�m@kC�@j��@j��@j�\@j�\@j^5@i��@i�#@i�^@i��@hĜ@h �@g��@g\)@g\)@gK�@g�@fȴ@fv�@fV@fE�@f$�@e�T@e��@e�-@e�h@e/@d�@d�D@d(�@c�m@c��@b�@b~�@b-@b-@b-@bM�@b~�@b�!@b�\@bJ@a�7@`r�@`b@_�;@_�w@_�w@_�w@_�w@_��@_;d@^�R@^{@]@]�@]p�@]`B@]?}@]V@\�@\��@\j@\Z@\I�@\9X@\1@[��@[��@[��@[t�@Z�!@Z=q@Z�@Z�@ZJ@Y�7@YX@YG�@X��@X�9@X��@X�u@XbN@X1'@X  @W��@Wl�@W
=@V�y@V�y@Vȴ@V�R@V��@VV@V@Up�@UO�@UO�@UO�@U�@T�@TZ@T(�@S�
@SdZ@R�H@R�!@Rn�@RJ@Q��@Qhs@Q�@Q&�@Q�@P�`@P�u@PbN@PQ�@PA�@PA�@P �@O�w@O|�@O;d@O�@O
=@Nȴ@Nff@N$�@N@M�@M�T@M�T@M@M@Mp�@MO�@M/@M�@L�@L��@Lj@L1@L1@K��@K"�@J��@J^5@I�@I��@IX@I&�@H��@H�u@H�@H �@H  @G�;@G��@G�w@G�@G�P@F�y@F�+@F5?@F@E�@D�@D�D@C�@CC�@B�H@B�\@BM�@B�@A��@A�#@Ax�@A&�@A�@A�@@�9@@�u@@bN@@ �@?�w@?|�@?K�@?+@>�R@>v�@>5?@>$�@>{@>@=�T@=��@=�@<�j@<Z@;��@;��@;t�@;dZ@;"�@:�!@:^5@:M�@:J@9�#@9�^@9��@9��@9��@9��@9�7@9�7@9G�@97L@9%@8�9@8bN@81'@8  @7�w@7�P@6�R@65?@5�@5O�@5?}@5�@4�/@4�j@4�D@4�@3�F@333@3@2�H@2��@2�\@2~�@2^5@2M�@2�@1�^@17L@0��@0��@0�@0�@0r�@0bN@0bN@0Q�@0b@/�;@/�w@/�@/��@/�P@/l�@.�R@.�+@.v�@.ff@.$�@-�T@-�-@-�-@-�h@-�@-p�@-O�@-O�@-O�@-?}@-O�@-/@-/@-/@-�@,��@,�@,�@,�/@,�/@,�/@,�/@,�@,1@+�@+"�@*�\@*=q@*-@)�@)�#@)�^@)x�@)7L@)�@(Ĝ@(�u@(A�@(b@'��@'�P@'|�@';d@'�@&�@&��@&ff@&V@&V@&E�@&5?@&5?@&5?@&{@%@%�h@%p�@%O�@%?}@%�@$�@$z�@$j@$Z@$I�@$�@#ƨ@#��@#dZ@#o@"�\@"M�@"�@!�7@!G�@!�@ ��@ r�@ A�@�@+@�@�+@v�@E�@�@��@�@/@�@�D@z�@Z@(�@1@1@��@�m@ƨ@��@S�@@^5@=q@J@�^@hs@Ĝ@�u@�@�@r�@bN@Q�@A�@1'@  @�@K�@�@v�@E�@�T@O�@��@��@��@��@�D@Z@Z@Z@Z@I�@I�@I�@I�@�@�F@t�@33@�@�H@��@��@~�@n�@M�@=q@J@��@7L@7L@�@��@Ĝ@�9@��@A�@  @��@�w@�w@��@�P@l�@\)@\)@K�@;d@+@�@�@�@�@�@��@@��@�-@��@�h@O�@�@�@V@�j@j@Z@9X@��@�
@�
@t�@o@@@o@"�@C�@@
�H@
�@
�@
~�@
M�@
M�@
=q@	�#@	�^@	��@	hs@	&�@�`@�u@bN@Q�@Q�@Q�@Q�@Q�@  @�w@�w@|�@|�@|�@l�@K�@+@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AÇ+AÇ+AÉ7AÍPAÏ\AËDAÉ7A�~�A�z�A�x�A�n�A�^5A�I�A�9XA��/A§�A�%A�&�A��A�jA�I�A��A�K�A�ZA��jA�&�A���A�l�A�33A�Q�A�1'A�|�A�
=A���A�33A�33A�~�A�1A�|�A�ƨA�+A��A�x�A�%A�VA��A�+A�|�A���A��A���A�$�A���A�K�A�VA�A�5?A�%A�~�A�JA�bA��+A�`BA�{A��wA���A��DA�M�A���A� �A��A���A�A��\A�33A~�+A}l�A{�PAz{Ay|�AyAxr�Aw�7Av�\Av  Au%At��AsƨAp�ApZAn�An$�AmƨAm�AlE�Ak"�AjAhffAf��Af�Ae�TAe&�Ac��Ab�Ab�A`�A_O�A\�A\9XAZ�yAZ  AY|�AX�uAWƨAWG�AV��AU�-AT��AT�+ATM�AS�mASt�AS�AR-AQ33AP�/AP�9APn�AO�AN�`AN(�AM��AM`BALn�AJ��AI��AIAG�AEAEAC��AC33AB$�AA��A@��A@bNA??}A>bA=�^A=t�A<�!A<bNA;ƨA:�HA:JA9�A9�A81'A7x�A6ȴA6ffA57LA4v�A41A4  A3��A3��A3;dA2�RA1��A0�A0bA/O�A-oA+�7A*�9A)�A)+A)oA)A(��A'�7A&��A&I�A%�hA$5?A"�A!�;A (�AG�A�AI�A�AoA-A��A"�AE�AoA��A��A~�AI�A��AS�Az�A�-A�+A  AXA��AA�A��A
��A	K�A �At�A��AVA�A�7A��A�HAȴAhsA��A�+A�@�$�@��u@�bN@�(�@�l�@��H@�/@� �@�ƨ@��@��@�D@�!@�$�@��m@�@�M�@睲@���@�x�@�I�@�t�@�M�@�z�@߾w@��@�=q@���@۾w@��@�%@�;d@��@��#@ԣ�@ҸR@��@���@У�@Ѓ@��
@͡�@̴9@�C�@��@ʧ�@�ff@��@���@�C�@�
=@�@�@���@ƸR@�n�@ļj@�|�@��@�v�@��^@���@�dZ@�V@��^@��@��F@�C�@���@�A�@��@�ff@�X@�1@�K�@��R@�{@�x�@��u@�j@�1@�\)@�@�ff@���@�O�@�Ĝ@��D@��@�dZ@��H@��R@���@���@��@�9X@��;@�t�@�;d@�
=@�v�@��#@�/@�Z@�\)@�ȴ@�E�@�@���@�`B@�%@�z�@��@��w@�o@��\@�=q@��@�`B@���@��@�9X@�l�@�K�@�C�@��y@�-@���@��7@�1'@��;@���@�dZ@���@���@���@�V@�=q@�{@�?}@���@��@�b@��F@�t�@�ȴ@�J@�`B@�X@�/@��@�bN@�1@���@�C�@��R@���@�$�@��@��-@�hs@��@��@��m@���@�ƨ@��w@��@�\)@��!@�^5@�5?@�p�@��@���@�Q�@�  @�S�@�E�@�$�@�{@�@���@��@��@��T@���@��^@��@�&�@��j@���@��D@��m@��@�|�@�l�@�\)@�33@��!@��+@�ff@�@�X@���@���@��D@�9X@�  @�@�;@l�@;d@+@
=@
=@
=@~��@~V@}�-@}�@}`B@}�@|�/@|1@{o@z~�@zJ@yX@x�`@xr�@xA�@w�w@wK�@w+@w+@w�@v��@v�@v�R@v��@vv�@vE�@v$�@u@uO�@t�j@tZ@t�@s��@s@rn�@q�#@q��@q��@q�^@q��@qhs@q7L@p��@p�u@o�;@o\)@o�@nȴ@n�+@nV@nE�@m�T@m�@m?}@l�/@k�m@kC�@j��@j��@j�\@j�\@j^5@i��@i�#@i�^@i��@hĜ@h �@g��@g\)@g\)@gK�@g�@fȴ@fv�@fV@fE�@f$�@e�T@e��@e�-@e�h@e/@d�@d�D@d(�@c�m@c��@b�@b~�@b-@b-@b-@bM�@b~�@b�!@b�\@bJ@a�7@`r�@`b@_�;@_�w@_�w@_�w@_�w@_��@_;d@^�R@^{@]@]�@]p�@]`B@]?}@]V@\�@\��@\j@\Z@\I�@\9X@\1@[��@[��@[��@[t�@Z�!@Z=q@Z�@Z�@ZJ@Y�7@YX@YG�@X��@X�9@X��@X�u@XbN@X1'@X  @W��@Wl�@W
=@V�y@V�y@Vȴ@V�R@V��@VV@V@Up�@UO�@UO�@UO�@U�@T�@TZ@T(�@S�
@SdZ@R�H@R�!@Rn�@RJ@Q��@Qhs@Q�@Q&�@Q�@P�`@P�u@PbN@PQ�@PA�@PA�@P �@O�w@O|�@O;d@O�@O
=@Nȴ@Nff@N$�@N@M�@M�T@M�T@M@M@Mp�@MO�@M/@M�@L�@L��@Lj@L1@L1@K��@K"�@J��@J^5@I�@I��@IX@I&�@H��@H�u@H�@H �@H  @G�;@G��@G�w@G�@G�P@F�y@F�+@F5?@F@E�@D�@D�D@C�@CC�@B�H@B�\@BM�@B�@A��@A�#@Ax�@A&�@A�@A�@@�9@@�u@@bN@@ �@?�w@?|�@?K�@?+@>�R@>v�@>5?@>$�@>{@>@=�T@=��@=�@<�j@<Z@;��@;��@;t�@;dZ@;"�@:�!@:^5@:M�@:J@9�#@9�^@9��@9��@9��@9��@9�7@9�7@9G�@97L@9%@8�9@8bN@81'@8  @7�w@7�P@6�R@65?@5�@5O�@5?}@5�@4�/@4�j@4�D@4�@3�F@333@3@2�H@2��@2�\@2~�@2^5@2M�@2�@1�^@17L@0��@0��@0�@0�@0r�@0bN@0bN@0Q�@0b@/�;@/�w@/�@/��@/�P@/l�@.�R@.�+@.v�@.ff@.$�@-�T@-�-@-�-@-�h@-�@-p�@-O�@-O�@-O�@-?}@-O�@-/@-/@-/@-�@,��@,�@,�@,�/@,�/@,�/@,�/@,�@,1@+�@+"�@*�\@*=q@*-@)�@)�#@)�^@)x�@)7L@)�@(Ĝ@(�u@(A�@(b@'��@'�P@'|�@';d@'�@&�@&��@&ff@&V@&V@&E�@&5?@&5?@&5?@&{@%@%�h@%p�@%O�@%?}@%�@$�@$z�@$j@$Z@$I�@$�@#ƨ@#��@#dZ@#o@"�\@"M�@"�@!�7@!G�@!�@ ��@ r�@ A�@�@+@�@�+@v�@E�@�@��@�@/@�@�D@z�@Z@(�@1@1@��@�m@ƨ@��@S�@@^5@=q@J@�^@hs@Ĝ@�u@�@�@r�@bN@Q�@A�@1'@  @�@K�@�@v�@E�@�T@O�@��@��@��@��@�D@Z@Z@Z@Z@I�@I�@I�@I�@�@�F@t�@33@�@�H@��@��@~�@n�@M�@=q@J@��@7L@7L@�@��@Ĝ@�9@��@A�@  @��@�w@�w@��@�P@l�@\)@\)@K�@;d@+@�@�@�@�@�@��@@��@�-@��@�h@O�@�@�@V@�j@j@Z@9X@��@�
@�
@t�@o@@@o@"�@C�@@
�H@
�@
�@
~�@
M�@
M�@
=q@	�#@	�^@	��@	hs@	&�@�`@�u@bN@Q�@Q�@Q�@Q�@Q�@  @�w@�w@|�@|�@|�@l�@K�@+@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�;B�B�B�B�NB��B�B��B��B�BZBG�B:^B33B)�B$�B�BuBB��B�B�B�fB�/B�
B��BŢB��B�LB�9B�B��B��B�JB�Bw�Bs�Bo�BcTB]/BS�BC�B8RB2-B#�B�B�BhBPB
=B1BB
��B
�B
�5B
��B
�wB
�?B
�B
��B
��B
�PB
� B
y�B
w�B
t�B
n�B
gmB
bNB
\)B
W
B
Q�B
=qB
9XB
/B
(�B
%�B
!�B
�B
uB
JB
B	��B	��B	�B	�B	�HB	�B	��B	��B	ŢB	�FB	�!B	�B	��B	��B	��B	��B	��B	�uB	�\B	�%B	�B	�B	|�B	{�B	z�B	u�B	ffB	cTB	aHB	cTB	dZB	`BB	\)B	XB	VB	P�B	B�B	?}B	>wB	8RB	,B	%�B	!�B	�B	�B	oB	hB	bB	bB	DB		7B	1B	1B	%B	B	B��B��B��B�B�B�B�B�mB�TB�HB�NB�`B�`B�ZB�BB�B��B��B��BȴB�}B�dB�LB�9B�3B�3B�-B�B�B��B��B��B��B�{B�VB�=B�7B�B~�B|�By�Bv�Bu�Br�Bn�Bm�Bm�Bl�Bk�Bk�BhsBgmBffBcTBbNB`BB^5B]/BYBT�BR�BM�BL�BJ�BH�BH�BF�BF�BE�BF�BF�BG�BD�BB�B?}B:^B5?B49B49B49B49B7LB9XB>wB=qB9XB9XB49B.B&�B$�B$�B"�B!�B!�B �B �B �B �B�B�B�B �B!�B!�B �B!�B"�B!�B#�B#�B!�B!�B"�B!�B!�B �B!�B$�B#�B#�B#�B#�B#�B$�B%�B&�B&�B'�B)�B)�B/B0!B0!B0!B1'B2-B49B49B5?B6FB6FB7LB9XB:^B;dB=qB@�BB�BC�BD�BE�BG�BH�BH�BH�BH�BH�BJ�BL�BM�BN�BO�BN�BN�BQ�BQ�BP�BR�BS�BS�BS�BVBXB]/B_;BaHBbNBcTBffBiyBk�Bl�Bl�Bn�Bn�Bo�Bq�Bs�Bt�Bu�Bv�Bw�Bz�B|�B|�B� B�B�B�B�1B�DB�JB�PB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�9B�9B�9B�FB�XB�dB�jB�wB��B��BĜBŢBƨBǮB��B��B��B��B��B��B��B��B�B�B�#B�NB�mB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B		7B	JB	JB	JB	PB	\B	uB	�B	�B	�B	�B	"�B	%�B	%�B	'�B	+B	,B	-B	33B	8RB	9XB	;dB	=qB	>wB	@�B	F�B	I�B	I�B	J�B	K�B	L�B	P�B	S�B	VB	W
B	XB	ZB	[#B	\)B	]/B	_;B	_;B	_;B	_;B	_;B	_;B	_;B	`BB	`BB	aHB	aHB	bNB	bNB	cTB	e`B	ffB	hsB	k�B	n�B	q�B	s�B	s�B	v�B	x�B	z�B	{�B	|�B	}�B	�B	�B	�B	�B	�%B	�+B	�+B	�=B	�DB	�JB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�FB	�RB	�^B	�jB	�jB	�qB	�wB	��B	��B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�/B	�/B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
JB
JB
JB
JB
PB
PB
VB
\B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
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
;dB
;dB
;dB
;dB
;dB
;dB
;dB
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
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
T�B
T�B
VB
W
B
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
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
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
^5B
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
aHB
aHB
aHB
bNB
bNB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
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
m�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B�B��B�B��B��B�B�B�(B�HB�B�EB�CB��B�RB�B��B�B��B��B��B��B�7B]�BJrB<�B5�B,"B'�BB�BEB��B�B�B�
B��B�KB�\B��B��B�RB�FB��B��B��B�<B��Bx�Bu?Bq�Bd�B_;BVSBEB9�B4B$�BB?B:B�B
�B	7B�B
��B
�aB
��B
�~B
��B
��B
�'B
�tB
��B
��B
��B
z�B
x�B
u�B
o�B
hXB
cnB
\�B
X�B
T�B
>�B
:�B
0!B
)�B
&�B
# B
/B
2B
<B
�B	��B	��B	��B	�/B	�hB	�QB	��B	�B	�B	�fB	��B	�)B	��B	��B	��B	�qB	�SB	��B	�HB	��B	�{B	��B	}�B	|�B	|B	v�B	gB	c�B	a�B	d�B	e,B	a-B	\�B	X�B	WsB	R�B	C�B	@�B	@iB	:^B	-)B	'8B	#B	�B	YB	uB	TB	�B	�B	�B		�B		B	�B	+B	9B	'B��B��B�B��B�B�}B�B�XB��B�|B�B��B�B�`B�B�WB�SB҉BӏBʌB��B��B��B��B��B�B��B�5B�B�DB��B��B�1B��B��B�0B�)B�B� B~Bz�Bw�BwBtBo Bm�Bm�BmBlWBl�Bi�Bh�Bg�Bd@BcTBaHB`'B_pB[WBW�BT�BOvBM�BK�BI�BIRBG_BGzBFBG_BHfBH�BE�BD�BA�B;dB5�B4�B4�B5B5ZB8B9�B?cB>�B:^B;B6�B/iB'�B&B&�B#�B"�B"�B!�B!�B!�B!bB �B vB �B!�B#B"�B!�B"�B#TB"�B%,B$�B"B"B#:B"�B#:B!�B"�B%,B$@B$ZB$tB$�B$�B%,B&B'B'B(sB*B+6B/�B0�B0�B0�B2B33B5B4�B6+B6�B6�B8lB:�B;JB<B>]BAoBCGBD3BEBF%BHKBIBIBI7BIBI7BKDBMBNVBO(BPbBO\BO\BR:BR:BQ�BS�BTFBTFBTaBVSBXyB]�B_�Ba�BcBdBgBi�Bk�Bl�Bl�Bo Bo5BpBr-Bt9Bu?Bv+Bw2BxRB{dB}"B}qB��B�GB�MB��B��B��B��B�"B��B��B��B��B��B��B�	B�B�!B�bB�NB�B�TB�FB�_B��B��B��B�nB�nB��B��B��B��B��B��B��B��B��B�B�B�1B�DB�(B��B��B��B�.B�NBԕB�BچB��B��B�B��B��B�IB�GB��B��B�B�B�B��B�B�B�0B�JB�VB�HB	'B	GB	�B		�B	dB	~B	~B	�B	�B	�B	�B	B	B	!B	# B	&B	&2B	($B	+6B	,"B	-CB	3MB	8lB	9rB	;B	=�B	>�B	@�B	F�B	I�B	I�B	J�B	K�B	M6B	Q4B	T,B	V9B	WsB	XEB	ZkB	[=B	\xB	]dB	_VB	_VB	_pB	_pB	_pB	_pB	_pB	`\B	`\B	a|B	a�B	b�B	b�B	c�B	e�B	f�B	h�B	k�B	n�B	q�B	s�B	s�B	v�B	y$B	z�B	|B	}<B	~BB	�;B	�AB	�MB	�SB	�YB	�EB	�_B	�rB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�*B	�0B	�0B	�0B	�B	�B	�"B	�CB	�IB	�OB	�AB	�|B	�hB	�nB	��B	��B	��B	�jB	��B	��B	�wB	��B	��B	��B	�B	�6B	� B	�B	�&B	�B	��B	�&B	�B	�&B	�,B	�YB	�1B	�QB	�=B	�=B	�dB	�dB	�dB	�dB	�VB	�BB	�\B	�bB	�|B	�NB	�NB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�<B	�BB	�B	�B
 4B
 OB
;B
 B
 B
'B
AB
-B
3B
B
3B
3B
3B
9B
?B
YB
YB
YB
_B
KB
	RB

XB

XB

XB

=B

rB

XB

rB
dB
~B
~B
dB
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
#B
"�B
#B
#B
#�B
#�B
$�B
%B
%B
&2B
'B
'B
'B
'B
(
B
($B
($B
(>B
)*B
*0B
*0B
+B
+6B
,"B
,=B
,=B
-)B
-CB
-)B
./B
./B
./B
.B
.B
.B
./B
.B
.IB
/5B
/OB
/OB
0UB
0;B
0;B
1AB
1vB
1�B
2|B
3hB
4TB
4TB
4TB
5ZB
5ZB
5ZB
6zB
6zB
7�B
7�B
7�B
8�B
8�B
8lB
8lB
9rB
9�B
9�B
9�B
:xB
:xB
:xB
:xB
:^B
:xB
:^B
:�B
:xB
;�B
;�B
;dB
;B
;�B
;B
;�B
=�B
=qB
=�B
=�B
>�B
>�B
>wB
>�B
>wB
>�B
>�B
>wB
>wB
>�B
>wB
>�B
?}B
?�B
?�B
?�B
?}B
?�B
?�B
?}B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
MB
M�B
M�B
OB
OB
OB
P.B
PB
Q B
QB
Q B
QB
R B
RB
RB
S&B
S&B
TB
TB
TB
T,B
T,B
S�B
TB
UB
U2B
UB
UB
U2B
V9B
W?B
W$B
W$B
WYB
W?B
XEB
XB
XB
XB
X+B
XB
XB
XEB
X+B
XEB
YKB
YeB
YKB
ZQB
ZkB
[qB
[WB
[#B
[#B
[=B
[qB
\]B
\CB
\)B
\)B
]/B
]IB
]/B
]/B
]IB
]IB
^OB
^OB
^jB
^5B
^OB
^jB
^jB
^OB
_VB
_pB
_VB
_pB
`\B
`BB
`\B
`\B
aHB
abB
abB
a|B
abB
b�B
bhB
bNB
bhB
bNB
cnB
cTB
cnB
cnB
cTB
cnB
c�B
dtB
dtB
dtB
d�B
d�B
d�B
d�B
d�B
dZB
d�B
dtB
dtB
cTB
cnB
c�B
dtB
dtB
dtB
d�B
dtB
d�B
d�B
e�B
ezB
e`B
ffB
ffB
f�B
g�B
g�B
gmB
hsB
h�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
jB
jB
jB
jB
jB
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�q�<��I<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607120037292016071200372920160712003729201806221210412018062212104120180622121041201804050403002018040504030020180405040300  JA  ARFMdecpA19c                                                                20160708093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160708003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160708003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160708003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160708003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160708003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160708003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160708003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160708003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160708003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20160708011829                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160708153502  CV  JULD            G�O�G�O�F�͟                JM  ARSQJMQC2.0                                                                 20160711000000  CF  PSAL_ADJUSTED_QCD�` D�` G�O�                JM  ARCAJMQC2.0                                                                 20160711153729  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160711153729  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190300  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031041  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101516                      G�O�G�O�G�O�                