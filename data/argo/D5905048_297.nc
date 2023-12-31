CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-01T00:35:44Z creation;2018-11-01T00:35:48Z conversion to V3.1;2019-12-19T07:25:12Z update;     
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
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20181101003544  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              )A   JA  I2_0577_297                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؍6|�0 1   @؍7l��@4T�j~���diO�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:�H@z�H@�=q@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�(�A�\)A�\)A�\)A�\)B�B�B�B�B'�B0{B8{B?G�BGG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3��C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�DHDz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB�{DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qD���D��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�z=D�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�@�D�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�+A�"�A��A�oA�JA�%A�A�A�A�%A�1A�
=A� �A�=qA�E�A�I�A�+A��Aٲ-A��A׃A�9XA���A�VA���A�Q�A�K�A���A� �A�A���A�%A���A���A��hA��
A��A���A�JA��+A���A�|�A��/A��PA�;dA��A���A��mA�33A��DA�1A�^5A�S�A�&�A�x�A�=qA�  A��-A�/A� �A��A��A��A�O�A��FA�
=A�G�A�;dA��A���A�M�A�1'A��TA���A�1'A�\)A���A� �A�Q�A���A�1A�l�A�ȴA���A�Q�A�mA~ �A|��A{|�Ax��At��Ao\)Al-Aj�Ai�-Ai7LAh$�Af��Ad$�A`��A_�A\9XAY�AX�AW�#AW7LAU�ASƨAR��AP�yAOS�AN��AN1'AK��AIAGO�AF �AEG�ADr�AB��A@�RA>~�A=��A<ffA9&�A8r�A8  A7�A4  A29XA1XA/��A.n�A,�A,�uA+K�A(�`A'�;A&r�A%�#A%%A#�mA#;dA#oA!�A I�AhsA�+A%Ar�A�;AQ�A�hA�A��A�\A�A��A��AC�A�hAI�A`BA%A��Av�A�A��A�yAVA��AK�A
ĜA
��A
ffA
JA	��A�AJAhsAA�HA�AVA ff@�J@��@���@�%@�  @��y@��T@���@�@��@���@�J@�o@�O�@��H@�x�@��m@�C�@�5?@�G�@���@�z�@ާ�@ݙ�@ܬ@��@�`B@�%@ם�@�ff@�-@Չ7@Ձ@�`B@���@�z�@�z�@�j@�bN@��H@�{@�X@�%@��
@�  @� �@�1@��@�^5@�J@��#@Ͳ-@�%@̃@��;@��@ə�@�9X@Ƨ�@�@���@���@þw@î@�t�@Å@���@��@¸R@\@�v�@�ff@��@���@��@�7L@���@�I�@���@��@�dZ@�K�@���@��!@�v�@�hs@��@�ƨ@�"�@���@�%@�I�@��;@�ȴ@�{@���@���@���@���@�j@��
@��P@�dZ@�@���@��\@�$�@�{@��T@�p�@�?}@�7L@��@���@��9@��@�Q�@� �@�  @��@���@�l�@�\)@�~�@��@�O�@���@�9X@��@�+@��H@���@�v�@�=q@���@���@���@�/@���@� �@��@���@�E�@�@��7@��7@�`B@�O�@�?}@��9@���@�Ĝ@�bN@�ƨ@�t�@�33@���@�v�@�-@��@�@�p�@�O�@�G�@�`B@�G�@�%@��j@��@�bN@�1'@�\)@��H@���@�v�@�$�@�@��@�@���@�hs@��@���@��@�A�@�1@��m@���@��F@���@���@���@�|�@�C�@��@��!@��+@�ff@��@��#@�x�@�7L@�%@���@��j@�z�@� �@���@��
@���@�C�@�+@�"�@�o@�@���@��+@�-@�@���@�@���@�&�@��`@��/@��@�z�@�9X@�ƨ@�t�@�C�@�C�@�33@���@���@���@�=q@��@��@��^@���@��h@��@�x�@�hs@�/@���@�z�@���@���@��@�C�@�"�@�"�@�"�@�"�@���@���@�^5@�5?@���@�hs@���@��/@���@��j@���@�z�@�bN@�Z@�9X@��
@���@�t�@�C�@��@�@���@���@�ff@�V@�=q@�-@�-@�J@���@���@�G�@�&�@�V@�%@���@��`@��j@��u@�r�@�Z@�Z@�bN@�I�@�1'@��
@���@�\)@�+@���@�$�@�@�{@��T@��T@��#@��^@���@��@��9@�Q�@�A�@�(�@�  @��
@���@�\)@���@�n�@�-@�@��@���@�p�@��@���@���@�z�@�9X@�1'@�@�;@|�@~ȴ@~��@~v�@~V@~{@}��@}��@}O�@}/@|��@|j@|z�@|Z@|I�@|9X@|9X@|I�@|�@{�
@{��@{t�@{o@z��@zJ@yG�@x�`@x�9@x�u@xb@vȴ@u�@u��@u?}@t�D@t1@s�@so@r�\@rJ@q�#@qx�@qhs@qG�@p��@pr�@o�w@o|�@o\)@o;d@n��@n@m�-@mp�@m�@l�/@lj@k��@kS�@j�H@j��@j��@i��@h�9@hA�@g�@g\)@g+@f�y@f��@f��@fff@e��@eO�@d�j@dI�@cƨ@co@bn�@a��@aX@a&�@`��@`A�@_;d@^��@^$�@]�-@]p�@]/@\9X@[C�@Zn�@Y��@YX@Y�@XĜ@Xr�@X �@X  @W�@WK�@V��@VV@U@T��@T��@T�D@TZ@T(�@Sƨ@S�@S"�@R�@Rn�@Q�@Q��@Qx�@QX@QG�@Q�@P�9@P1'@O�P@O+@N��@N��@N$�@M�@M`B@L�/@L�j@L��@Lj@L1@KC�@J��@JM�@I��@I��@I��@Ihs@I7L@I%@HĜ@H�u@H1'@G�@G�P@GK�@G+@G
=@Fȴ@F{@E�@E�T@E@E@E�-@E��@E�h@E`B@E?}@D�j@Dj@D9X@D1@Cƨ@CC�@C@B�H@B��@B~�@B=q@A��@Ahs@AX@AG�@A7L@A�@@��@@��@@Ĝ@@r�@@  @?��@?\)@?K�@?;d@?;d@>��@>ȴ@>v�@=�@=@=O�@<�D@<I�@;�
@;��@;dZ@;o@:��@:~�@9�@9��@9�7@9G�@9&�@97L@8��@8��@8�9@8�9@8�9@8�9@8r�@8  @7�@7�P@7+@6�R@6E�@6@5��@5`B@5V@4�@4��@4��@4Z@3�m@3�@3C�@2�\@2M�@1�@1�7@1x�@1%@0�`@0�`@0��@0��@0�9@01'@/�;@/l�@/+@.�y@.�+@.V@.$�@.{@-�@-��@-�@,�j@,�D@,j@,I�@,1@+�m@+��@+"�@*�!@*n�@*^5@*-@)��@)�@)��@)�@(�u@(�@(bN@(1'@(  @'�;@'�w@'|�@'l�@'+@&�R@&V@&E�@&{@%��@%p�@$��@$I�@$(�@$�@#��@#�
@#��@#S�@#"�@"��@"�!@"=q@!��@!��@!�^@!��@!�7@!x�@!hs@!G�@!&�@!&�@!%@ �9@ r�@ b@�P@�@�y@�@�R@ff@5?@$�@$�@�T@`B@��@�@�D@(�@�m@ƨ@��@�@dZ@C�@o@o@�H@�!@~�@^5@-@��@��@&�@�9@Q�@A�@  @�@��@�w@��@;d@;d@�@
=@��@�@��@ff@5?@$�@��@��@�h@�@`B@`B@`B@?}@V@�@�D@I�@(�@��@ƨ@��@��@��@t�@C�@o@�@@@�H@��@^5@=q@J@��@�@�#@��@�^@��@x�@hs@7L@��@Ĝ@�u@bN@ �@b@�;@��@\)@+@�@
=@��@v�@ff@5?@$�@{@�T@@�@p�@`B@`B@O�@?}@�@��@�@�/@�D@Z@I�@�@��@��@�@S�@o@@
�!@
�\@
n�@
=q@
�@
J@	�@	��@	�7@	hs@	G�@	G�@	&�@	�@	%@��@��@Ĝ@��@�@�@�@�@�@�@�@bN@bN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�+A�"�A��A�oA�JA�%A�A�A�A�%A�1A�
=A� �A�=qA�E�A�I�A�+A��Aٲ-A��A׃A�9XA���A�VA���A�Q�A�K�A���A� �A�A���A�%A���A���A��hA��
A��A���A�JA��+A���A�|�A��/A��PA�;dA��A���A��mA�33A��DA�1A�^5A�S�A�&�A�x�A�=qA�  A��-A�/A� �A��A��A��A�O�A��FA�
=A�G�A�;dA��A���A�M�A�1'A��TA���A�1'A�\)A���A� �A�Q�A���A�1A�l�A�ȴA���A�Q�A�mA~ �A|��A{|�Ax��At��Ao\)Al-Aj�Ai�-Ai7LAh$�Af��Ad$�A`��A_�A\9XAY�AX�AW�#AW7LAU�ASƨAR��AP�yAOS�AN��AN1'AK��AIAGO�AF �AEG�ADr�AB��A@�RA>~�A=��A<ffA9&�A8r�A8  A7�A4  A29XA1XA/��A.n�A,�A,�uA+K�A(�`A'�;A&r�A%�#A%%A#�mA#;dA#oA!�A I�AhsA�+A%Ar�A�;AQ�A�hA�A��A�\A�A��A��AC�A�hAI�A`BA%A��Av�A�A��A�yAVA��AK�A
ĜA
��A
ffA
JA	��A�AJAhsAA�HA�AVA ff@�J@��@���@�%@�  @��y@��T@���@�@��@���@�J@�o@�O�@��H@�x�@��m@�C�@�5?@�G�@���@�z�@ާ�@ݙ�@ܬ@��@�`B@�%@ם�@�ff@�-@Չ7@Ձ@�`B@���@�z�@�z�@�j@�bN@��H@�{@�X@�%@��
@�  @� �@�1@��@�^5@�J@��#@Ͳ-@�%@̃@��;@��@ə�@�9X@Ƨ�@�@���@���@þw@î@�t�@Å@���@��@¸R@\@�v�@�ff@��@���@��@�7L@���@�I�@���@��@�dZ@�K�@���@��!@�v�@�hs@��@�ƨ@�"�@���@�%@�I�@��;@�ȴ@�{@���@���@���@���@�j@��
@��P@�dZ@�@���@��\@�$�@�{@��T@�p�@�?}@�7L@��@���@��9@��@�Q�@� �@�  @��@���@�l�@�\)@�~�@��@�O�@���@�9X@��@�+@��H@���@�v�@�=q@���@���@���@�/@���@� �@��@���@�E�@�@��7@��7@�`B@�O�@�?}@��9@���@�Ĝ@�bN@�ƨ@�t�@�33@���@�v�@�-@��@�@�p�@�O�@�G�@�`B@�G�@�%@��j@��@�bN@�1'@�\)@��H@���@�v�@�$�@�@��@�@���@�hs@��@���@��@�A�@�1@��m@���@��F@���@���@���@�|�@�C�@��@��!@��+@�ff@��@��#@�x�@�7L@�%@���@��j@�z�@� �@���@��
@���@�C�@�+@�"�@�o@�@���@��+@�-@�@���@�@���@�&�@��`@��/@��@�z�@�9X@�ƨ@�t�@�C�@�C�@�33@���@���@���@�=q@��@��@��^@���@��h@��@�x�@�hs@�/@���@�z�@���@���@��@�C�@�"�@�"�@�"�@�"�@���@���@�^5@�5?@���@�hs@���@��/@���@��j@���@�z�@�bN@�Z@�9X@��
@���@�t�@�C�@��@�@���@���@�ff@�V@�=q@�-@�-@�J@���@���@�G�@�&�@�V@�%@���@��`@��j@��u@�r�@�Z@�Z@�bN@�I�@�1'@��
@���@�\)@�+@���@�$�@�@�{@��T@��T@��#@��^@���@��@��9@�Q�@�A�@�(�@�  @��
@���@�\)@���@�n�@�-@�@��@���@�p�@��@���@���@�z�@�9X@�1'@�@�;@|�@~ȴ@~��@~v�@~V@~{@}��@}��@}O�@}/@|��@|j@|z�@|Z@|I�@|9X@|9X@|I�@|�@{�
@{��@{t�@{o@z��@zJ@yG�@x�`@x�9@x�u@xb@vȴ@u�@u��@u?}@t�D@t1@s�@so@r�\@rJ@q�#@qx�@qhs@qG�@p��@pr�@o�w@o|�@o\)@o;d@n��@n@m�-@mp�@m�@l�/@lj@k��@kS�@j�H@j��@j��@i��@h�9@hA�@g�@g\)@g+@f�y@f��@f��@fff@e��@eO�@d�j@dI�@cƨ@co@bn�@a��@aX@a&�@`��@`A�@_;d@^��@^$�@]�-@]p�@]/@\9X@[C�@Zn�@Y��@YX@Y�@XĜ@Xr�@X �@X  @W�@WK�@V��@VV@U@T��@T��@T�D@TZ@T(�@Sƨ@S�@S"�@R�@Rn�@Q�@Q��@Qx�@QX@QG�@Q�@P�9@P1'@O�P@O+@N��@N��@N$�@M�@M`B@L�/@L�j@L��@Lj@L1@KC�@J��@JM�@I��@I��@I��@Ihs@I7L@I%@HĜ@H�u@H1'@G�@G�P@GK�@G+@G
=@Fȴ@F{@E�@E�T@E@E@E�-@E��@E�h@E`B@E?}@D�j@Dj@D9X@D1@Cƨ@CC�@C@B�H@B��@B~�@B=q@A��@Ahs@AX@AG�@A7L@A�@@��@@��@@Ĝ@@r�@@  @?��@?\)@?K�@?;d@?;d@>��@>ȴ@>v�@=�@=@=O�@<�D@<I�@;�
@;��@;dZ@;o@:��@:~�@9�@9��@9�7@9G�@9&�@97L@8��@8��@8�9@8�9@8�9@8�9@8r�@8  @7�@7�P@7+@6�R@6E�@6@5��@5`B@5V@4�@4��@4��@4Z@3�m@3�@3C�@2�\@2M�@1�@1�7@1x�@1%@0�`@0�`@0��@0��@0�9@01'@/�;@/l�@/+@.�y@.�+@.V@.$�@.{@-�@-��@-�@,�j@,�D@,j@,I�@,1@+�m@+��@+"�@*�!@*n�@*^5@*-@)��@)�@)��@)�@(�u@(�@(bN@(1'@(  @'�;@'�w@'|�@'l�@'+@&�R@&V@&E�@&{@%��@%p�@$��@$I�@$(�@$�@#��@#�
@#��@#S�@#"�@"��@"�!@"=q@!��@!��@!�^@!��@!�7@!x�@!hs@!G�@!&�@!&�@!%@ �9@ r�@ b@�P@�@�y@�@�R@ff@5?@$�@$�@�T@`B@��@�@�D@(�@�m@ƨ@��@�@dZ@C�@o@o@�H@�!@~�@^5@-@��@��@&�@�9@Q�@A�@  @�@��@�w@��@;d@;d@�@
=@��@�@��@ff@5?@$�@��@��@�h@�@`B@`B@`B@?}@V@�@�D@I�@(�@��@ƨ@��@��@��@t�@C�@o@�@@@�H@��@^5@=q@J@��@�@�#@��@�^@��@x�@hs@7L@��@Ĝ@�u@bN@ �@b@�;@��@\)@+@�@
=@��@v�@ff@5?@$�@{@�T@@�@p�@`B@`B@O�@?}@�@��@�@�/@�D@Z@I�@�@��@��@�@S�@o@@
�!@
�\@
n�@
=q@
�@
J@	�@	��@	�7@	hs@	G�@	G�@	&�@	�@	%@��@��@Ĝ@��@�@�@�@�@�@�@�@bN@bN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��BBhB�B)�B;dBQ�BO�BM�B{�Bp�BI�BL�B��B�BƨBB��B�B�yBBVB�B��B+B��B  B�BVB��B�B��B\B%B�`B��B��B��B�B��B�!B��B��B��B�!B��B�B�JBp�BaHBZBS�BN�BH�B1'B�B
�B
�qB
�'B
�bB
�hB
r�B
?}B
Q�B
_;B
T�B
B�B
2-B
L�B
J�B
6FB
�B
�B
JB
1B	��B	�B	ɺB	�B	�B	�PB	��B	��B	��B	�B	s�B	T�B	E�B	<jB	2-B	,B	/B	49B	,B	 �B	DB	hB	B��B	B��B�NB��B�B�
B�B��BĜB�9B�'B�FB�B�oB��B��B��B|�B�B�hB�%B�B�B�DB|�Bn�By�Bx�B� B}�Bx�By�B�Br�BffBm�Bk�BdZBm�Bm�B^5Bk�Bk�Bv�Bp�BcTBS�B\)BYBVBbNBjBv�Bx�Bu�Bs�Bp�Bn�Bo�Bt�Bp�Br�By�Bv�Bq�Bn�BjBdZBgmB\)B\)B^5BM�BN�BI�BT�BVBbNB`BBgmBn�Bo�Bm�Bq�Bl�B_;BO�B\)BN�B[#BYBcTBaHBbNBffBcTBYBXBR�BZBL�B\)BXBW
BaHBaHBhsBiyBiyBhsBm�Bn�Bm�Be`BjBo�Br�Br�B�B�hB�\B�JB��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�wBBBĜBÖBȴB��B��B��B�
B�B�B�NB�B�B�B�B��B��B��B��B��B��B�B�B��B��B��B��B��B	B	B	
=B	{B	�B	�B	�B	!�B	!�B	$�B	(�B	&�B	'�B	+B	+B	0!B	0!B	/B	49B	7LB	7LB	6FB	6FB	9XB	:^B	;dB	=qB	?}B	>wB	>wB	?}B	;dB	<jB	E�B	G�B	N�B	N�B	T�B	VB	W
B	YB	XB	XB	YB	XB	VB	W
B	YB	YB	[#B	^5B	aHB	aHB	ffB	gmB	hsB	iyB	jB	s�B	u�B	v�B	w�B	z�B	|�B	{�B	|�B	}�B	~�B	�B	� B	�B	�B	�+B	�1B	�+B	�=B	�PB	�\B	�PB	�PB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�FB	�FB	�LB	�XB	�jB	�jB	�qB	��B	B	B	B	B	B	ÖB	ƨB	ȴB	ɺB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�)B	�)B	�5B	�BB	�NB	�NB	�HB	�BB	�5B	�;B	�BB	�BB	�5B	�HB	�fB	�mB	�fB	�fB	�mB	�sB	�sB	�mB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B	��B
  B
B
B
B
B
%B
%B
1B
+B
B
	7B
	7B
	7B
	7B
1B
	7B
	7B

=B
DB

=B
\B
bB
bB
hB
hB
hB
hB
hB
hB
hB
bB
bB
hB
bB
uB
{B
{B
oB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
�B
!�B
!�B
$�B
&�B
&�B
'�B
)�B
)�B
'�B
(�B
(�B
+B
+B
)�B
,B
+B
-B
.B
.B
,B
+B
.B
0!B
1'B
1'B
0!B
-B
-B
/B
1'B
1'B
49B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
49B
5?B
5?B
9XB
9XB
9XB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
<jB
<jB
<jB
<jB
:^B
:^B
:^B
=qB
=qB
=qB
<jB
=qB
=qB
=qB
@�B
@�B
?}B
>wB
=qB
>wB
@�B
@�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
D�B
C�B
D�B
D�B
C�B
B�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
D�B
G�B
G�B
H�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
F�B
H�B
I�B
I�B
I�B
H�B
G�B
G�B
G�B
H�B
G�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
O�B
O�B
N�B
O�B
N�B
M�B
M�B
N�B
M�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
O�B
Q�B
Q�B
R�B
S�B
R�B
T�B
VB
VB
VB
T�B
S�B
T�B
T�B
W
B
W
B
W
B
YB
YB
ZB
ZB
YB
XB
YB
[#B
\)B
[#B
[#B
[#B
[#B
ZB
[#B
\)B
]/B
]/B
]/B
]/B
\)B
[#B
\)B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
^5B
_;B
aHB
`BB
_;B
`BB
_;B
aHB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
ffB
ffB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
e`B
dZB
e`B
dZB
e`B
e`B
gmB
hsB
hsB
gmB
hsB
hsB
hsB
gmB
ffB
gmB
hsB
iyB
hsB
iyB
jB
jB
jB
jB
jB
jB
k�B
jB
jB
k�B
k�B
jB
jB
jB
hsB
jB
k�B
m�B
l�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
n�B
o�B
o�B
p�B
o�B
p�B
p�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
q�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
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
s�B
t�B
u�B
u�B
v�B
v�B
u�B
v�B
u�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
v�B
x�B
x�B
x�B
w�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
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
|�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
�B
�B
��B
�B
�B
�B
�B
��B
��B�BhB�B*eB<BR�BQ�BP�B}VBu�BR�BWYB��B�B�PB�B��BߊB��B�B4B��B�B
=B��B{B�BbB�B�B�B.B�B�B��B�xB�lB�}B��B��B�B��B�FB�AB��B��B��Bs�BdtB\�BVmBPbBJXB3hB�B
��B
�uB
�9B
�@B
��B
vB
EB
S�B
`�B
VSB
DgB
3�B
L�B
LJB
88B
 �B
B
\B

XB
 �B	��B	��B	�!B	��B	��B	�B	�+B	�yB	��B	vB	X�B	IlB	>�B	5�B	.�B	0�B	5ZB	-CB	"�B	B	�B	SB��B	B�]B�B�
B�	BخB�KBԕB��B�2B��B��B�/B�9B��B��B��B��B�GB��B�1B�?B��B�B~�Bq[B{JBz�B��B.Bz^Bz�B��BtnBhXBn�Bl�BfLBn}Bn�B`\Bl�BlqBwLBqvBd�BV�B]�B[=BXyBc�Bk�BwLBy>Bv`BtTBqvBo�BpoButBq�BshBzBwLBraBoiBk�Be�Bh�B^B]�B_�BP}BQNBK�BV�BWYBb�Ba-Bh>Bo�Bp�Bo Br|Bm�BaHBR B]dBPbB\BZ7Bc�BbBb�Bf�Bc�BZQBX�BS�BZ�BN�B\�BYBW�Ba|Ba�Bh�Bi�Bi�Bh�Bm�Bn�Bm�Bf�Bk6Bp!BsBs�B�B�hB��B�PB��B��B��B�B�CB�/B�IB�xB��B��B��B��B��B��B��B��B��BĶB��B��B�(B�&B�B�?B�mB�yB�B�B��B�B�!B��B��B��B�B�B�B�B�B�RB��B�xB��B��B	�B	�B	
�B	�B	�B	�B	 B	!�B	"4B	%FB	)B	'RB	(XB	+6B	+QB	0;B	0UB	/iB	4nB	7fB	7fB	6zB	6�B	9�B	:�B	;�B	=�B	?�B	>�B	>�B	?�B	<B	=B	FB	HKB	OBB	OvB	UMB	V9B	W?B	Y1B	XEB	XEB	YKB	X_B	VmB	W�B	YB	Y�B	[�B	^�B	a�B	a�B	f�B	g�B	h�B	i�B	j�B	s�B	vB	wB	xRB	{0B	}"B	|PB	}"B	~(B	.B	� B	�iB	�-B	�9B	�+B	�KB	�_B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�B	�$B	�$B	�B	�0B	�B	�B	�B	�*B	�0B	�=B	�/B	�OB	�OB	�UB	�vB	�nB	�tB	�`B	�zB	�zB	��B	��B	��B	��B	��B	��B	ªB	ªB	ªB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�&B	�B	� B	�:B	�4B	�2B	�MB	�9B	�+B	�+B	�+B	�EB	�EB	�9B	�mB	�YB	�sB	�CB	�xB	�jB	�\B	�NB	�NB	�bB	�vB	�jB	�pB	��B	��B	ޞB	�B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�$B	�*B	�?B	�	B	��B	�"B
  B	�B	�B	�<B	�rB	�"B	�6B
 B	�.B	�.B	�.B	�BB	�VB	�VB	�dB
UB
'B
GB
AB	�cB
 4B
;B
GB
MB
MB
YB
YB
KB
_B
�B
	RB
	lB
	RB
	RB
KB
	lB
	lB

XB
xB

rB
vB
}B
bB
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 �B
 �B
!�B
 �B
B
B
!�B
"B
$�B
'B
'B
($B
*B
*B
(>B
)*B
)DB
+QB
+QB
*KB
,=B
+QB
-CB
./B
.IB
,qB
+�B
.IB
0UB
1[B
1AB
0UB
-wB
-wB
/iB
1[B
1vB
4TB
5ZB
5ZB
5tB
6zB
5tB
5tB
5tB
4�B
5�B
5�B
9rB
9rB
9�B
9�B
8�B
9rB
9rB
9rB
9�B
9�B
:�B
<�B
<�B
<�B
<�B
:�B
:�B
:�B
=�B
=�B
=�B
<�B
=�B
=�B
=�B
@�B
@�B
?�B
>�B
=�B
>�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
D�B
C�B
D�B
D�B
C�B
B�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
D�B
G�B
G�B
H�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
F�B
H�B
I�B
I�B
I�B
H�B
G�B
G�B
G�B
H�B
G�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
K�B
K�B
MB
M�B
M�B
M�B
NB
O�B
O�B
N�B
O�B
N�B
NB
M�B
OB
NB
MB
NB
N�B
PB
P.B
PB
QB
Q B
Q B
PB
PB
Q B
Q B
PB
R B
R:B
S&B
T,B
S&B
UB
VB
VB
VB
UB
T,B
UB
U2B
W$B
W?B
W?B
Y1B
Y1B
Z7B
Z7B
YKB
XEB
Y1B
[WB
\CB
[=B
[=B
[=B
[=B
ZQB
[qB
\]B
]/B
]IB
]IB
]IB
\CB
[WB
\]B
_;B
_VB
_VB
_VB
_VB
_VB
_pB
`BB
_pB
^�B
_pB
aHB
`\B
_VB
`�B
_pB
a|B
cTB
cnB
cnB
cnB
b�B
b�B
bhB
cnB
cnB
c�B
c�B
ffB
f�B
e�B
ezB
ffB
f�B
f�B
f�B
f�B
e�B
d�B
ezB
d�B
e�B
e�B
g�B
hsB
h�B
g�B
h�B
h�B
hsB
g�B
f�B
g�B
h�B
i�B
h�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
j�B
j�B
k�B
k�B
j�B
j�B
j�B
h�B
j�B
k�B
m�B
l�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
n�B
o�B
o�B
p�B
o�B
p�B
p�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
q�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
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
s�B
t�B
u�B
u�B
v�B
v�B
u�B
v�B
u�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
v�B
w�B
w�B
xB
v�B
x�B
x�B
y	B
w�B
zB
x�B
zB
y�B
y�B
y�B
y�B
y�B
zB
z�B
{B
z�B
{�B
|B
|B
{�B
{�B
|B
{�B
|B
|B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811050041302018110500413020181105004130201811050200252018110502002520181105020025201811060025382018110600253820181106002538  JA  ARFMdecpA19c                                                                20181101093531  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181101003544  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181101003547  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181101003547  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181101003548  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181101003548  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181101003548  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181101003548  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181101003548  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181101003548                      G�O�G�O�G�O�                JA  ARUP                                                                        20181101005743                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181101153356  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20181104154130  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181104154130  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181104170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181105152538  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                