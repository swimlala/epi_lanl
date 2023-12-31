CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-15T00:36:14Z creation;2017-11-15T00:36:17Z conversion to V3.1;2019-12-19T07:56:40Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20171115003614  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_179                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�5u��s�1   @�5w^З�@;��PH�dfi�B��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�=q@�p�A�RA@Q�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B({B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bx{B�B��
B��
B��
B���B��
B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOt{DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��=D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�@�D׀�D׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�r�A�r�A�r�A�v�A�t�A�t�A�t�A�v�A�v�A�t�A�v�A�v�A�r�A�r�A�t�A�r�A�r�A�l�A�Q�A�7LA���Aȕ�A��mA�  Aŝ�A���A�XA�$�A���A¾wA���A�\)A�ffA��^A���A� �A�9XA��7A��uA��-A��!A��A�&�A�~�A�ĜA�A��+A��A�ZA��mA�z�A���A��-A�I�A�$�A���A���A�
=A��yA��7A�p�A��-A�VA��HA��A�7LA�S�A�ZA�G�A�/A�bNA���A�r�A�ffA�\)A�M�A�(�A�VA��mA�t�A��wA|�A~r�A}��A}�A}�^A|-Az��Az��Ay�TAx��Aw�-Aw�Av�/Av�9AvffAu�AtVAr��Ar�Aq�AqG�Ap�jAp(�Ao�PAn��Am�#Aj�HAiAi��Ai�AhjAg�
Ag�wAg��Ag�AfZAe33AbZAa��Aa/A`ffA`1A_�PA_�A^I�A\��A[�AZĜAZ-AY`BAXr�AW�AW��AW\)AVr�AU�#AT�HATQ�AS�#AR�!AQAP^5AOƨAOS�ANI�AM��AM��AM�PAM�AM�AM�AMC�AL�RAK��AKVAJjAJ1'AI�AI�FAI
=AH�+AF��AD�AC��AB�jAA��AA33A@�A@1'A?%A>r�A>$�A=�A<�A;�7A:�`A:5?A9�A9S�A9VA8r�A7/A65?A4jA3x�A3�A2�HA2�!A2M�A1"�A/�A.��A.9XA,v�A*��A*-A)��A);dA(�A(��A(jA'
=A&A$�A#��A"��A"z�A!�TA!K�A �jA��A7LA�9A�AXA�9A�#A+AM�AdZA�DA�mAK�A��A;dAn�A��A7LA�jA~�AAz�A�mAp�A`BAS�AVAn�AƨA�A��A��A�-A
��A
��A
~�A	��A�yA��A&�A�HA1'A(�AI�At�A ff@���@�"�@���@���@�/@�ƨ@�E�@��@��P@��@�S�@�M�@�u@�@�1'@�S�@���@柾@�/@��@߅@��y@�-@�O�@ܬ@� �@ە�@�;d@�"�@�o@��T@ش9@�9X@�K�@�r�@���@҇+@���@ϝ�@���@̛�@� �@��
@ˮ@�S�@�5?@���@���@���@�O�@���@��@��@���@��@�Q�@��@��@�O�@���@�33@�ȴ@�V@��\@�@��h@�x�@�G�@�V@�%@���@��/@��j@���@�z�@�A�@��
@��@�^5@�x�@��@��@�v�@���@�V@�ƨ@��@�@�n�@��u@�K�@�@�~�@��@��h@�/@�Ĝ@��@��
@���@�@�V@��@��T@�&�@��u@��@��F@�|�@�@���@�n�@�E�@���@���@���@�b@��@�ȴ@�E�@��#@�O�@��@�bN@��m@�;d@�J@�x�@�`B@���@���@�I�@�1'@�b@�  @���@��P@�|�@�t�@�
=@��+@�n�@�n�@�ff@�ff@�=q@�5?@�5?@�{@�r�@�1@��m@��;@��m@���@�  @�  @���@�33@���@��@�%@��;@�t�@�1@�b@��m@���@��P@�+@��!@��\@�hs@���@��
@��y@��!@��@�p�@�O�@�&�@��@�Q�@�1'@�Z@�G�@�?}@���@�bN@�z�@�  @��@��+@��^@��9@�Q�@~��@~@}p�@|�@|�D@|�@|�D@{��@{ƨ@z��@yX@xbN@x �@w|�@u�@u�T@t��@s��@s�F@s@r��@r�\@r~�@rM�@q��@q�#@q�^@q�^@q�^@q�7@qhs@qG�@q�@p��@p�9@p�u@p��@p��@p�u@p�@p�@p�@p�@p�u@p��@p��@p�u@p1'@o�w@o|�@o�@n�y@n�R@n�+@n$�@m�-@mp�@mV@l�D@lZ@l9X@k�
@k�F@k�F@k��@kdZ@kC�@ko@j�@j��@j��@j�@j�@j�@j��@jn�@j-@i��@i��@iG�@i&�@hĜ@hr�@h �@g�@g�@g\)@fV@e��@e��@e�@e`B@eV@d�j@dj@d9X@d1@d1@d(�@dI�@dZ@d1@ct�@b�@b��@b��@b�\@bJ@a��@aG�@`�`@`Ĝ@`�`@a�^@co@c33@c33@`Ĝ@`1'@^V@]O�@]/@\��@\�@]O�@_;d@`A�@`�u@`�9@`�@` �@`  @_l�@^�y@^��@]�h@\j@\(�@\1@[��@[�F@[t�@Z��@Z��@Z^5@Y�@Y�7@Y%@X�`@X�`@XbN@W��@Vv�@U`B@T�@Tz�@S�m@S��@S��@S��@S��@S"�@R�!@R��@R�\@Rn�@R�@Q�@Q�7@P�u@O�;@O\)@N�y@NE�@N@M�T@M�T@N$�@NE�@Nff@N$�@M�@MV@L��@L�@K"�@JM�@I�^@Ihs@IG�@I&�@I�@I�@I�@I%@H��@H��@H �@G|�@G
=@F�+@D�/@Dz�@Dz�@DZ@Ct�@B�H@B�!@B�!@B�\@Bn�@B�\@B�@A�@@Q�@?�@?�;@?\)@>�@>v�@>$�@=�-@=�@=p�@=?}@<��@<��@<�D@<z�@<z�@;�
@;C�@;@:�@:�@:�H@:=q@9��@9�^@9�7@9X@9�@8Ĝ@8��@8 �@7�w@7�w@7�w@7l�@7l�@7��@7��@7��@7l�@7;d@6�y@6��@6v�@6V@6$�@6{@6{@6@5��@5��@5?}@4�@4�D@49X@3�
@3S�@2�H@2��@2�!@2^5@2�@2J@1��@1X@1%@0��@0�`@0Ĝ@0�u@0Q�@/�@/��@/�P@/+@.ȴ@.�R@.v�@.$�@-�-@-p�@-?}@,��@,�/@,�j@,��@,�D@,j@,I�@,I�@,(�@,�@+�
@+�F@+��@+dZ@+"�@+@+@*�@*��@*�\@*n�@*^5@*M�@*M�@*=q@*=q@*-@)�@)�^@)�7@)x�@)7L@(��@(�9@(�@(A�@(  @'�;@'\)@'�@&�@&E�@%�T@%�-@%p�@%?}@%V@$�@$�/@$��@$�j@$�@$Z@$�@#�m@#��@#t�@#S�@#33@#"�@#@"�H@"=q@!�#@!�^@!��@!�7@!x�@!%@ ��@ bN@ Q�@ 1'@�@�w@�P@|�@l�@;d@�@ȴ@5?@��@�@/@�@�/@��@��@�D@z�@j@9X@�m@ƨ@S�@o@@�@�@�!@�\@J@��@��@��@�7@x�@�7@�7@�7@��@�7@�7@hs@�@�`@r�@�@|�@\)@l�@K�@��@5?@�T@��@�h@�h@�@`B@`B@��@�j@��@�D@�D@��@�j@��@�m@C�@S�@"�@��@�!@�!@��@��@��@�\@M�@=q@-@�#@��@X@G�@�@Ĝ@Q�@A�@A�@A�@1'@ �@�@K�@;d@+@
=@��@�+@ff@E�@�@�-@��@�h@p�@�@�/@I�@�@��@��@�m@�
@ƨ@��@t�@S�@o@
�@
��@
��@
��@
^5@
�@	x�@��@Ĝ@��@�u@�u@�@Q�@ �@ �@ �@ �@  @�;@�@�P@l�@\)@;d@;d@+@��@�y@ȴ@��@��@v�@E�@$�@{@@�T@��@��@��@�h@�h@p�@?}@��@�/@�j@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�r�A�r�A�r�A�v�A�t�A�t�A�t�A�v�A�v�A�t�A�v�A�v�A�r�A�r�A�t�A�r�A�r�A�l�A�Q�A�7LA���Aȕ�A��mA�  Aŝ�A���A�XA�$�A���A¾wA���A�\)A�ffA��^A���A� �A�9XA��7A��uA��-A��!A��A�&�A�~�A�ĜA�A��+A��A�ZA��mA�z�A���A��-A�I�A�$�A���A���A�
=A��yA��7A�p�A��-A�VA��HA��A�7LA�S�A�ZA�G�A�/A�bNA���A�r�A�ffA�\)A�M�A�(�A�VA��mA�t�A��wA|�A~r�A}��A}�A}�^A|-Az��Az��Ay�TAx��Aw�-Aw�Av�/Av�9AvffAu�AtVAr��Ar�Aq�AqG�Ap�jAp(�Ao�PAn��Am�#Aj�HAiAi��Ai�AhjAg�
Ag�wAg��Ag�AfZAe33AbZAa��Aa/A`ffA`1A_�PA_�A^I�A\��A[�AZĜAZ-AY`BAXr�AW�AW��AW\)AVr�AU�#AT�HATQ�AS�#AR�!AQAP^5AOƨAOS�ANI�AM��AM��AM�PAM�AM�AM�AMC�AL�RAK��AKVAJjAJ1'AI�AI�FAI
=AH�+AF��AD�AC��AB�jAA��AA33A@�A@1'A?%A>r�A>$�A=�A<�A;�7A:�`A:5?A9�A9S�A9VA8r�A7/A65?A4jA3x�A3�A2�HA2�!A2M�A1"�A/�A.��A.9XA,v�A*��A*-A)��A);dA(�A(��A(jA'
=A&A$�A#��A"��A"z�A!�TA!K�A �jA��A7LA�9A�AXA�9A�#A+AM�AdZA�DA�mAK�A��A;dAn�A��A7LA�jA~�AAz�A�mAp�A`BAS�AVAn�AƨA�A��A��A�-A
��A
��A
~�A	��A�yA��A&�A�HA1'A(�AI�At�A ff@���@�"�@���@���@�/@�ƨ@�E�@��@��P@��@�S�@�M�@�u@�@�1'@�S�@���@柾@�/@��@߅@��y@�-@�O�@ܬ@� �@ە�@�;d@�"�@�o@��T@ش9@�9X@�K�@�r�@���@҇+@���@ϝ�@���@̛�@� �@��
@ˮ@�S�@�5?@���@���@���@�O�@���@��@��@���@��@�Q�@��@��@�O�@���@�33@�ȴ@�V@��\@�@��h@�x�@�G�@�V@�%@���@��/@��j@���@�z�@�A�@��
@��@�^5@�x�@��@��@�v�@���@�V@�ƨ@��@�@�n�@��u@�K�@�@�~�@��@��h@�/@�Ĝ@��@��
@���@�@�V@��@��T@�&�@��u@��@��F@�|�@�@���@�n�@�E�@���@���@���@�b@��@�ȴ@�E�@��#@�O�@��@�bN@��m@�;d@�J@�x�@�`B@���@���@�I�@�1'@�b@�  @���@��P@�|�@�t�@�
=@��+@�n�@�n�@�ff@�ff@�=q@�5?@�5?@�{@�r�@�1@��m@��;@��m@���@�  @�  @���@�33@���@��@�%@��;@�t�@�1@�b@��m@���@��P@�+@��!@��\@�hs@���@��
@��y@��!@��@�p�@�O�@�&�@��@�Q�@�1'@�Z@�G�@�?}@���@�bN@�z�@�  @��@��+@��^@��9@�Q�@~��@~@}p�@|�@|�D@|�@|�D@{��@{ƨ@z��@yX@xbN@x �@w|�@u�@u�T@t��@s��@s�F@s@r��@r�\@r~�@rM�@q��@q�#@q�^@q�^@q�^@q�7@qhs@qG�@q�@p��@p�9@p�u@p��@p��@p�u@p�@p�@p�@p�@p�u@p��@p��@p�u@p1'@o�w@o|�@o�@n�y@n�R@n�+@n$�@m�-@mp�@mV@l�D@lZ@l9X@k�
@k�F@k�F@k��@kdZ@kC�@ko@j�@j��@j��@j�@j�@j�@j��@jn�@j-@i��@i��@iG�@i&�@hĜ@hr�@h �@g�@g�@g\)@fV@e��@e��@e�@e`B@eV@d�j@dj@d9X@d1@d1@d(�@dI�@dZ@d1@ct�@b�@b��@b��@b�\@bJ@a��@aG�@`�`@`Ĝ@`�`@a�^@co@c33@c33@`Ĝ@`1'@^V@]O�@]/@\��@\�@]O�@_;d@`A�@`�u@`�9@`�@` �@`  @_l�@^�y@^��@]�h@\j@\(�@\1@[��@[�F@[t�@Z��@Z��@Z^5@Y�@Y�7@Y%@X�`@X�`@XbN@W��@Vv�@U`B@T�@Tz�@S�m@S��@S��@S��@S��@S"�@R�!@R��@R�\@Rn�@R�@Q�@Q�7@P�u@O�;@O\)@N�y@NE�@N@M�T@M�T@N$�@NE�@Nff@N$�@M�@MV@L��@L�@K"�@JM�@I�^@Ihs@IG�@I&�@I�@I�@I�@I%@H��@H��@H �@G|�@G
=@F�+@D�/@Dz�@Dz�@DZ@Ct�@B�H@B�!@B�!@B�\@Bn�@B�\@B�@A�@@Q�@?�@?�;@?\)@>�@>v�@>$�@=�-@=�@=p�@=?}@<��@<��@<�D@<z�@<z�@;�
@;C�@;@:�@:�@:�H@:=q@9��@9�^@9�7@9X@9�@8Ĝ@8��@8 �@7�w@7�w@7�w@7l�@7l�@7��@7��@7��@7l�@7;d@6�y@6��@6v�@6V@6$�@6{@6{@6@5��@5��@5?}@4�@4�D@49X@3�
@3S�@2�H@2��@2�!@2^5@2�@2J@1��@1X@1%@0��@0�`@0Ĝ@0�u@0Q�@/�@/��@/�P@/+@.ȴ@.�R@.v�@.$�@-�-@-p�@-?}@,��@,�/@,�j@,��@,�D@,j@,I�@,I�@,(�@,�@+�
@+�F@+��@+dZ@+"�@+@+@*�@*��@*�\@*n�@*^5@*M�@*M�@*=q@*=q@*-@)�@)�^@)�7@)x�@)7L@(��@(�9@(�@(A�@(  @'�;@'\)@'�@&�@&E�@%�T@%�-@%p�@%?}@%V@$�@$�/@$��@$�j@$�@$Z@$�@#�m@#��@#t�@#S�@#33@#"�@#@"�H@"=q@!�#@!�^@!��@!�7@!x�@!%@ ��@ bN@ Q�@ 1'@�@�w@�P@|�@l�@;d@�@ȴ@5?@��@�@/@�@�/@��@��@�D@z�@j@9X@�m@ƨ@S�@o@@�@�@�!@�\@J@��@��@��@�7@x�@�7@�7@�7@��@�7@�7@hs@�@�`@r�@�@|�@\)@l�@K�@��@5?@�T@��@�h@�h@�@`B@`B@��@�j@��@�D@�D@��@�j@��@�m@C�@S�@"�@��@�!@�!@��@��@��@�\@M�@=q@-@�#@��@X@G�@�@Ĝ@Q�@A�@A�@A�@1'@ �@�@K�@;d@+@
=@��@�+@ff@E�@�@�-@��@�h@p�@�@�/@I�@�@��@��@�m@�
@ƨ@��@t�@S�@o@
�@
��@
��@
��@
^5@
�@	x�@��@Ĝ@��@�u@�u@�@Q�@ �@ �@ �@ �@  @�;@�@�P@l�@\)@;d@;d@+@��@�y@ȴ@��@��@v�@E�@$�@{@@�T@��@��@��@�h@�h@p�@?}@��@�/@�j@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�5B�HB��B�B1'B8RB1'B�B�dBo�B$�B�BBDB�mBB�\B�-B�-B��B��B��B��B�{B�oB�7B�Bt�BdZBL�BS�BVBQ�BI�BA�B6FB$�B�B�B�BJB
��B
�B
�yB
�)B
��B
B
�jB
�FB
�RB
�XB
�RB
�FB
�9B
�'B
�B
��B
��B
�VB
�JB
�=B
�=B
�B
z�B
o�B
q�B
k�B
aHB
]/B
[#B
[#B
XB
VB
N�B
C�B
9XB
;dB
8RB
5?B
0!B
+B
%�B
�B
�B
B
B
B
B	��B	��B	��B	��B	�B	�B	�TB	��B	��B	��B	��B	ȴB	ƨB	��B	�^B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�DB	�+B	�B	{�B	u�B	p�B	n�B	m�B	gmB	gmB	jB	iyB	iyB	iyB	gmB	dZB	`BB	\)B	XB	VB	VB	S�B	Q�B	M�B	H�B	@�B	2-B	1'B	.B	)�B	&�B	%�B	!�B	�B	�B	�B	�B	bB	
=B	+B	B	B	B	  B��B�B�B�fB�`B�`B�ZB�NB�/B��B��B��BȴB�qB�RB�XB�dB�^B�XB�RB�3B��B��B��B��B��B��B��B��B��B�oB�uB�hB�VB�=B�+B�B� B|�Bx�Bu�Bu�Bq�Bn�BdZBffBffBffBhsBgmBbNBXB`BB^5BaHB`BB]/BYBW
BZBXBR�BK�BP�BO�BP�BK�BA�B?}B@�B@�B8RB,B&�B1'B-B49B5?B5?B49B33B.B+B)�B+B)�B#�B&�B �B�B"�B$�B �B{BhB-B/B2-B33B33B49B5?B6FB7LB7LB6FB1'B1'B33B/B%�B.B,B'�B+B+B&�B1'B2-B2-B/B-B/B33B33B49B0!B8RB?}B=qB;dB6FB8RB9XB>wB;dBA�BA�B;dB8RBC�BE�BF�BG�BF�BG�BG�BG�BF�BF�BF�BF�BE�BD�BE�BE�BD�BB�BJ�BK�BK�BM�BR�BT�BQ�BM�BQ�BXBXBXBYB\)B_;BaHBiyBiyBk�Bk�Bp�Bt�Bt�Bu�Bu�Bw�Bz�B|�B}�B� B�B�B�B�1B�+B�DB��B��B��B��B��B��B��B��B�B�9B�jB��BƨB��B��B��B��B��B�B�B�B�B�#B�/B�/B�/B�/B�/B�5B�BB�5B�#B�mB�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	1B	+B	+B	+B	+B	1B	PB		7B	+B	+B	%B	PB	\B	hB	oB	uB	oB	�B	�B	�B	#�B	'�B	'�B	)�B	0!B	0!B	/B	,B	-B	.B	49B	5?B	9XB	<jB	=qB	@�B	E�B	F�B	F�B	G�B	G�B	G�B	J�B	N�B	N�B	O�B	VB	VB	YB	_;B	`BB	cTB	ffB	ffB	ffB	gmB	iyB	jB	k�B	k�B	k�B	l�B	n�B	n�B	p�B	r�B	s�B	u�B	v�B	v�B	w�B	w�B	x�B	y�B	{�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�JB	�JB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�3B	�3B	�9B	�?B	�?B	�LB	�LB	�RB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�jB	�wB	��B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�)B	�BB	�BB	�BB	�BB	�BB	�HB	�BB	�BB	�HB	�NB	�HB	�ZB	�`B	�fB	�fB	�fB	�fB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
1B
1B
	7B
DB
PB
PB
PB
VB
bB
\B
VB
bB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
&�B
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
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
.B
-B
-B
/B
.B
.B
.B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
33B
33B
33B
49B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
9XB
:^B
:^B
;dB
:^B
;dB
;dB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
F�B
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
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
K�B
L�B
K�B
K�B
L�B
M�B
M�B
N�B
O�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
S�B
T�B
T�B
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
W
B
W
B
W
B
XB
W
B
VB
VB
XB
YB
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
^5B
^5B
^5B
]/B
^5B
^5B
_;B
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
aHB
aHB
aHB
aHB
bNB
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
ffB
ffB
gmB
ffB
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
hsB
hsB
iyB
iyB
jB
jB
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B�B��B��B��B�B�B��B��B��B��B��B��B�B�MB�mB��B�=B��B�vB�B��BeB1�B9�B4�B)BňBz�B/�B�VB�B<B�B��B��B�FB��B��B��B�CB�7B��B��B�)B��Bx8Bh
BQ4BT�BVmBR�BJ�BCGB9	B'�B!BWB�B"B
��B
��B
�B
ޞB
�uB
�9B
�BB
��B
��B
�rB
�lB
��B
��B
��B
��B
�2B
�qB
��B
�jB
��B
�rB
��B
|�B
q'B
r-B
l�B
b�B
^jB
[�B
[�B
X_B
V�B
O�B
EmB
;B
<PB
8�B
5�B
0�B
+�B
&�B
 �B
EB
YB
'B
�B
�B	��B	�xB	�B	�>B	��B	��B	�,B	��B	��B	уB	ˬB	�lB	�_B	�[B	��B	��B	�CB	�*B	��B	��B	��B	�VB	�IB	�7B	��B	�NB	�~B	��B	�B	}�B	w2B	rGB	oiB	ncB	h�B	h$B	j�B	i�B	i�B	i�B	g�B	d�B	aB	]/B	Y1B	V�B	VmB	TaB	R�B	N�B	I�B	B�B	4�B	2aB	/OB	+B	'�B	&�B	"�B	�B	QB	1B	�B	hB	�B	B	B	�B	�B	 �B�B�nB�!B�sB�fB��B��B��B�5B��B͹B��B�	B��B�*B�xB�6B��B��B��B�B��B�`B�HB�B��B�VB��B��B�yB��B�aB�:B�BB�^B�KB�gB� B~BBz*Bv�Bv�Br�Bo�Bf2Bg�BgmBgBi*Bh
BcnBY�BaB^�Ba|B`�B]�BZBW�BZkBX�BT,BM6BQ�BP�BQhBL�BC-B@�BA�BA;B9�B.�B)_B2GB.}B4�B5�B5�B5B3�B/5B,=B+QB,"B+�B%zB'�B"4B�B$B%�B"4B
B,B-wB/�B2�B3�B3�B4�B5�B6�B7�B7�B6�B2-B2B3�B0;B'�B.�B-)B)*B+�B+�B(XB1�B2|B2|B/�B-�B0!B4B4TB5�B1�B8�B?�B=�B;�B7fB9>B:^B?.B<�BBBBAB<�B9�BC�BE�BF�BG�BF�BG�BG�BG�BF�BF�BF�BF�BF?BE9BF?BFYBEmBC�BK)BLdBL�BN�BSuBUMBR�BOBBR�BXyBXyBXyBYB\�B_�Ba�Bi�Bi�Bl"Bl"Bp�BuBuZBvFBv+BxB{JB}qB~BB�4B�[B��B��B��B��B�B��B��B�B�/B�HB�8B��B��B��B��B��B��B�B�)B�B� B�&B�aB�1B�7B�QB�B�qB�dB�dB�IB�IB�~B�jB�vBޞB�CB��B��B��B��B��B��B�B�<B�dB�JB��B��B��B	;B	�B	KB	zB	_B	_B	�B	�B	�B	
	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#nB	(
B	(XB	*0B	0;B	0�B	/�B	,�B	-�B	.�B	4�B	6B	9�B	<�B	=�B	@�B	E�B	F�B	F�B	G�B	HB	H1B	KB	O(B	O(B	PbB	VB	VmB	Y�B	_VB	`�B	cnB	ffB	f�B	f�B	g�B	i�B	j�B	k�B	k�B	k�B	l�B	n�B	n�B	p�B	r�B	s�B	u�B	v�B	v�B	w�B	w�B	x�B	y�B	|B	|�B	}�B	~(B	~BB	�4B	�'B	�aB	�9B	�?B	�_B	�fB	�lB	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�8B	�
B	�*B	�0B	�B	�"B	�)B	�CB	�IB	�wB	�vB	�GB	�hB	�hB	�MB	�hB	�nB	�tB	�ZB	�fB	�LB	�lB	�lB	��B	��B	��B	��B	�xB	�xB	��B	��B	��B	��B	ĜB	ǔB	ʦB	�G�O�G�O�G�O�B	�"B	�jB	�B	�B	�B	��B	��B	ۦG�O�B	�'B	�\B	�\B	�vB	�bB	��B	��B	�|B	�B	��B	�tB	�zB	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�8B	�>B	�B	�0B	�0B	�B	�(B
  B
 B
'B
'B
[B
oB
AB
[B
UB
 iB	�HB
;B
;B
AB
AB
-B
3B
B
MB
9B
SB
mB
mB
SB
mB
�B
_B
	7B
KB
�B
	lB
^B
jB
jB
jB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
&B
'B
'B
'B
(
B
(
B
(
B
'�B
(
B
(
B
($B
($B
'B
(
B
($B
($B
($B
)*B
)DB
*B
*0B
*0B
*B
+6B
+B
*KB
+6B
,B
,"B
,=B
,"B
,"B
,WB
-CB
.IB
-CB
-CB
/5B
./B
.IB
.IB
0;B
1[B
1[B
2GB
2GB
2aB
3MB
3MB
3MB
49B
3MB
3MB
3hB
4TB
5ZB
5tB
6`B
7fB
7fB
7fB
7fB
7�B
8�B
9rB
9XB
9rB
9rB
:^B
:xB
9rB
:xB
:xB
;B
:�B
;B
;B
<�B
<�B
<�B
=�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
IB
I�B
J�B
J�B
J�B
J�B
J�B
KB
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
MB
K�B
L�B
LB
LB
MB
NB
M�B
OB
P.B
O(B
N�B
O�B
O�B
O�B
O�B
PB
O�B
O�B
O�B
QB
Q B
P�B
Q�B
Q�B
R B
Q4B
R:B
S�B
UB
UB
VB
VB
VB
VB
VB
VB
V9B
W
B
W?B
W$B
W?B
W?B
XB
W?B
VB
VB
X+B
YB
YB
Y1B
YKB
YKB
Y1B
ZB
ZQB
Z7B
ZQB
[WB
\]B
\CB
\CB
]dB
^5B
^jB
^OB
]~B
^OB
^�B
_pB
`\B
`BB
`BB
`\B
`BB
`vB
`\B
`\B
`\B
abB
abB
a|B
abB
a|B
abB
a|B
b�B
dtB
dtB
dZB
dZB
dtB
d�B
d�B
ezB
e`B
e`B
ezB
e�B
ezB
ezB
f�B
f�B
f�B
f�B
ffB
f�B
ffB
f�B
f�B
gmB
f�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
hsB
hsB
h�B
h�B
h�B
h�B
i�B
i�B
jB
j�B
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</Q<*d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711190034082017111900340820171119003408201806221233312018062212333120180622123331201804050429272018040504292720180405042927  JA  ARFMdecpA19c                                                                20171115093538  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171115003614  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171115003616  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171115003616  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171115003617  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171115003617  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171115003617  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171115003617  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171115003617  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171115003617                      G�O�G�O�G�O�                JA  ARUP                                                                        20171115010112                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171115153733  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171116000000  CF  PSAL_ADJUSTED_QCD�� D�@ G�O�                JM  ARCAJMQC2.0                                                                 20171118153408  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171118153408  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192927  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033331  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                