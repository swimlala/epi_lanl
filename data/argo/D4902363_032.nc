CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-31T00:35:14Z creation;2016-08-31T00:35:16Z conversion to V3.1;2019-12-19T08:31:41Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20160831003514  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL                A   JA  I2_0576_032                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��3��c�1   @��4\�$ @;H��@��dgDg8~1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)B {B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
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
C�C�C�C�C	�CC�C�C�CC�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@t{D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[t{D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��=D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:=D�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�:=D�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�:=D�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��=D�:=D�}qDؽqD��qD�=qD�}qDٽqD��=D�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD�
=D�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�A�1A�JA�JA�1A�%A�%A�1A�
=A�1A�JA�JA�JA�JA��mAҾwA�|�A�ffAЁA��#A�`BA��mA�XAœuA�ffAÙ�A�?}A�&�A���A�{A��+A�"�A��9A��yA��PA�ȴA�t�A���A�z�A��A��yA�x�A��!A�%A��FA�bNA�&�A�7LA�E�A�C�A�+A��A���A�7LA���A��A�^5A���A�/A��-A���A�/A���A��A�v�A��mA�bNA�C�A��A��!A���A��A�1'A�"�A�E�A~��A}��A|�A|��A{��A{%Az�AyVAw��Au�Au�At�`As�;ArȴAr��AqC�An�`An��AnI�Am�TAm?}Al�AlA�Ai�Ah��Ah-Ag�Ag�7Af��Af��AeK�AeVAdĜAd  Ab��Aap�Aa?}A`E�A`-A_��A_�-A^��A^Q�A]t�A\�RA\�+A\E�A[C�AZ=qAY��AYO�AXȴAWXAU&�AS�FASVAR�\AQ�#AQ;dAO�AN�+AN�AM�AM�#AMC�AL�\AJv�AI�#AIx�AGVAFJAE"�AD�9AD�AB�`AB(�AA�AAA@A�A?�A>�+A=?}A<1A;��A9��A8bNA7oA6 �A4�!A41'A3�A3�wA3t�A2�A0��A0(�A/�mA.ĜA.�+A-��A-
=A,n�A,1A+��A*��A*(�A)�A)��A)oA(ȴA(r�A'"�A%��A%K�A$�A$  A#C�A"5?A!7LA ��A  �A�A$�A�#A�wA�A7LA�!A�A�FAhsA�A��A5?AoAjA��AoAn�A�A�A7LA9XAE�A��A��Az�A�A��A(�AVA
bA	VA7LA�DAA�A�^AC�AjA9XAJA��A �@�l�@���@��m@�$�@���@�&�@���@�A�@�ƨ@�l�@�33@��H@�$�@�r�@��@�R@��@�bN@�C�@�V@�@��@�z�@�~�@�7L@�u@��@��#@߾w@�"�@��y@�n�@��@��@�r�@��@ׅ@�;d@֟�@�Z@Ѳ-@� �@�ff@�`B@˾w@ɺ^@��
@�+@�-@��#@�X@�j@��@��
@�;d@�@�@��T@���@�j@��@�p�@�A�@��P@��@��@�A�@��
@���@���@��^@�`B@��u@�"�@���@�O�@�j@�|�@��@��@���@�M�@�@�x�@��@���@�z�@�Ĝ@�bN@�A�@���@�ȴ@��y@��!@��\@�ff@�$�@�7L@��@�9X@���@�33@�o@��@���@�v�@�n�@�@�x�@���@�9X@��w@��P@�dZ@���@���@�E�@���@�?}@�V@�9X@���@���@�E�@�$�@���@��`@��9@��@�b@��!@��#@��@�(�@���@�
=@�ff@���@��^@���@��@���@�j@�(�@��;@�33@�@�@��R@�-@��@���@�p�@�Ĝ@�Z@�1'@��@�;d@���@���@�hs@��`@��j@�z�@�1'@� �@���@��@�ƨ@�t�@�+@��!@�=q@�-@�$�@��-@�`B@�O�@��9@��@�Q�@��@���@��@��@���@�~�@�n�@�^5@�M�@�5?@�$�@�@���@�X@�7L@��@��@���@�z�@�Q�@�9X@�1'@�  @��
@�r�@��`@���@��@�(�@��@���@��;@�|�@�l�@�dZ@�K�@�+@�
=@��R@�J@��-@�O�@��@�%@���@���@���@��@���@�(�@K�@~�y@~V@}�-@}`B@|�/@|�@|�D@|z�@|9X@{C�@z��@z��@z�@z�@z�H@zJ@y��@y�@y�#@y�@x�`@x�9@x�u@xbN@xA�@w�@xb@w�@w�@vȴ@v�R@vV@v{@u�@u�@u��@u@u�-@up�@u?}@tj@t1@s��@r�H@r~�@rn�@r^5@r=q@q�^@p��@p��@p�@p1'@o�@o�@p  @o�@o�;@o��@o�@ol�@nȴ@n��@nff@m��@mO�@m/@l�@l��@lz�@lZ@k�
@k33@j�H@j��@j�\@jJ@i��@i��@i�@hĜ@h �@g�@g�P@gK�@g
=@f��@e�h@e/@e�@eV@d�@d1@c�
@c��@b�!@b=q@a��@ahs@ax�@ahs@a&�@`�`@`bN@`b@_��@_|�@_;d@^V@]@]��@]��@]�@]O�@]V@\�j@\�@\j@[�F@[33@[dZ@[33@Z�H@Z��@[@[33@Z��@ZM�@ZJ@Y��@YX@Y%@XA�@X  @W�@W�;@W�w@WK�@V�+@U�@T�/@T�D@TZ@S��@Sƨ@S�@R��@RJ@Q�#@QX@P�9@PA�@O��@O\)@O+@N�R@N�+@N�+@Nff@N{@M@M��@Mp�@L�j@LZ@K��@K��@KdZ@KdZ@Ko@J��@J��@J��@Jn�@I�#@Ix�@IX@I�@H�9@H�u@H1'@H �@G�;@G;d@F��@G
=@G
=@F��@F��@F$�@E�@E��@Ep�@EV@D��@DI�@C�m@C��@C�@CdZ@C"�@B�@B�@B��@B��@B�@A�#@A��@A%@@�@@bN@@Q�@@A�@@A�@?�@?�@?|�@?K�@>�y@>�R@>��@>E�@=�@=��@=�@<�@<��@<I�@;ƨ@;t�@;@:�\@:=q@9��@9��@9�7@9G�@9%@8��@8Q�@8 �@8 �@8  @7�w@7�P@7;d@7�@7
=@6��@6@5@5`B@4��@4�D@4Z@3�m@3��@3��@3C�@2�@2�!@2�\@2n�@2^5@2=q@2�@2J@1�#@1�^@1��@1x�@1G�@0��@0�9@0�u@0�@0b@/��@/+@.�R@.�+@.ff@.E�@.E�@.$�@-p�@-?}@,��@,�@,�@,��@-�@-�@,��@,I�@,�@+�F@+�@+dZ@+@*�@*n�@)��@)x�@)X@)X@)X@)�@(�`@(��@(Q�@'�@'��@'�P@'|�@'|�@'|�@'\)@&�y@&�@&��@&E�@&@%��@%�-@%p�@%/@$�j@$��@$�@#�
@#��@#��@#�@#t�@#S�@#"�@"��@"~�@"=q@!�@!��@!&�@ �9@ �u@ r�@ r�@ bN@ Q�@ A�@ 1'@ b@�@�@�@�;@\)@�@�@��@E�@�T@�h@V@�/@�D@�D@�D@Z@1@S�@"�@�@��@�!@M�@��@��@hs@G�@G�@X@7L@%@��@�9@�@A�@b@�@�@�;@�w@��@l�@;d@
=@�R@v�@E�@@��@@�-@�-@��@��@�h@�h@�@�@p�@`B@`B@O�@�@��@��@�D@I�@9X@(�@�@�@1@��@�@C�@33@�@��@^5@��@�^@��@��@x�@%@�u@Q�@1'@ �@ �@b@�w@l�@\)@;d@�@
=@
=@��@��@$�@�@?}@?}@/@��@�D@z�@Z@I�@I�@I�@I�@�@��@�F@��@dZ@C�@
�@
��@
��@
�\@
M�@
-@	�#@	G�@	�@��@�`@�9@�u@r�@A�@b@�@��@��@|�@\)@K�@;d@+@��@��@�y@ȴ@�+@V@V@5?@@@{@{@@@@�@�@�T@�T@�T@��@�-@`B@��@��@�j@�D@I�@�@�
@t�@S�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�A�1A�JA�JA�1A�%A�%A�1A�
=A�1A�JA�JA�JA�JA��mAҾwA�|�A�ffAЁA��#A�`BA��mA�XAœuA�ffAÙ�A�?}A�&�A���A�{A��+A�"�A��9A��yA��PA�ȴA�t�A���A�z�A��A��yA�x�A��!A�%A��FA�bNA�&�A�7LA�E�A�C�A�+A��A���A�7LA���A��A�^5A���A�/A��-A���A�/A���A��A�v�A��mA�bNA�C�A��A��!A���A��A�1'A�"�A�E�A~��A}��A|�A|��A{��A{%Az�AyVAw��Au�Au�At�`As�;ArȴAr��AqC�An�`An��AnI�Am�TAm?}Al�AlA�Ai�Ah��Ah-Ag�Ag�7Af��Af��AeK�AeVAdĜAd  Ab��Aap�Aa?}A`E�A`-A_��A_�-A^��A^Q�A]t�A\�RA\�+A\E�A[C�AZ=qAY��AYO�AXȴAWXAU&�AS�FASVAR�\AQ�#AQ;dAO�AN�+AN�AM�AM�#AMC�AL�\AJv�AI�#AIx�AGVAFJAE"�AD�9AD�AB�`AB(�AA�AAA@A�A?�A>�+A=?}A<1A;��A9��A8bNA7oA6 �A4�!A41'A3�A3�wA3t�A2�A0��A0(�A/�mA.ĜA.�+A-��A-
=A,n�A,1A+��A*��A*(�A)�A)��A)oA(ȴA(r�A'"�A%��A%K�A$�A$  A#C�A"5?A!7LA ��A  �A�A$�A�#A�wA�A7LA�!A�A�FAhsA�A��A5?AoAjA��AoAn�A�A�A7LA9XAE�A��A��Az�A�A��A(�AVA
bA	VA7LA�DAA�A�^AC�AjA9XAJA��A �@�l�@���@��m@�$�@���@�&�@���@�A�@�ƨ@�l�@�33@��H@�$�@�r�@��@�R@��@�bN@�C�@�V@�@��@�z�@�~�@�7L@�u@��@��#@߾w@�"�@��y@�n�@��@��@�r�@��@ׅ@�;d@֟�@�Z@Ѳ-@� �@�ff@�`B@˾w@ɺ^@��
@�+@�-@��#@�X@�j@��@��
@�;d@�@�@��T@���@�j@��@�p�@�A�@��P@��@��@�A�@��
@���@���@��^@�`B@��u@�"�@���@�O�@�j@�|�@��@��@���@�M�@�@�x�@��@���@�z�@�Ĝ@�bN@�A�@���@�ȴ@��y@��!@��\@�ff@�$�@�7L@��@�9X@���@�33@�o@��@���@�v�@�n�@�@�x�@���@�9X@��w@��P@�dZ@���@���@�E�@���@�?}@�V@�9X@���@���@�E�@�$�@���@��`@��9@��@�b@��!@��#@��@�(�@���@�
=@�ff@���@��^@���@��@���@�j@�(�@��;@�33@�@�@��R@�-@��@���@�p�@�Ĝ@�Z@�1'@��@�;d@���@���@�hs@��`@��j@�z�@�1'@� �@���@��@�ƨ@�t�@�+@��!@�=q@�-@�$�@��-@�`B@�O�@��9@��@�Q�@��@���@��@��@���@�~�@�n�@�^5@�M�@�5?@�$�@�@���@�X@�7L@��@��@���@�z�@�Q�@�9X@�1'@�  @��
@�r�@��`@���@��@�(�@��@���@��;@�|�@�l�@�dZ@�K�@�+@�
=@��R@�J@��-@�O�@��@�%@���@���@���@��@���@�(�@K�@~�y@~V@}�-@}`B@|�/@|�@|�D@|z�@|9X@{C�@z��@z��@z�@z�@z�H@zJ@y��@y�@y�#@y�@x�`@x�9@x�u@xbN@xA�@w�@xb@w�@w�@vȴ@v�R@vV@v{@u�@u�@u��@u@u�-@up�@u?}@tj@t1@s��@r�H@r~�@rn�@r^5@r=q@q�^@p��@p��@p�@p1'@o�@o�@p  @o�@o�;@o��@o�@ol�@nȴ@n��@nff@m��@mO�@m/@l�@l��@lz�@lZ@k�
@k33@j�H@j��@j�\@jJ@i��@i��@i�@hĜ@h �@g�@g�P@gK�@g
=@f��@e�h@e/@e�@eV@d�@d1@c�
@c��@b�!@b=q@a��@ahs@ax�@ahs@a&�@`�`@`bN@`b@_��@_|�@_;d@^V@]@]��@]��@]�@]O�@]V@\�j@\�@\j@[�F@[33@[dZ@[33@Z�H@Z��@[@[33@Z��@ZM�@ZJ@Y��@YX@Y%@XA�@X  @W�@W�;@W�w@WK�@V�+@U�@T�/@T�D@TZ@S��@Sƨ@S�@R��@RJ@Q�#@QX@P�9@PA�@O��@O\)@O+@N�R@N�+@N�+@Nff@N{@M@M��@Mp�@L�j@LZ@K��@K��@KdZ@KdZ@Ko@J��@J��@J��@Jn�@I�#@Ix�@IX@I�@H�9@H�u@H1'@H �@G�;@G;d@F��@G
=@G
=@F��@F��@F$�@E�@E��@Ep�@EV@D��@DI�@C�m@C��@C�@CdZ@C"�@B�@B�@B��@B��@B�@A�#@A��@A%@@�@@bN@@Q�@@A�@@A�@?�@?�@?|�@?K�@>�y@>�R@>��@>E�@=�@=��@=�@<�@<��@<I�@;ƨ@;t�@;@:�\@:=q@9��@9��@9�7@9G�@9%@8��@8Q�@8 �@8 �@8  @7�w@7�P@7;d@7�@7
=@6��@6@5@5`B@4��@4�D@4Z@3�m@3��@3��@3C�@2�@2�!@2�\@2n�@2^5@2=q@2�@2J@1�#@1�^@1��@1x�@1G�@0��@0�9@0�u@0�@0b@/��@/+@.�R@.�+@.ff@.E�@.E�@.$�@-p�@-?}@,��@,�@,�@,��@-�@-�@,��@,I�@,�@+�F@+�@+dZ@+@*�@*n�@)��@)x�@)X@)X@)X@)�@(�`@(��@(Q�@'�@'��@'�P@'|�@'|�@'|�@'\)@&�y@&�@&��@&E�@&@%��@%�-@%p�@%/@$�j@$��@$�@#�
@#��@#��@#�@#t�@#S�@#"�@"��@"~�@"=q@!�@!��@!&�@ �9@ �u@ r�@ r�@ bN@ Q�@ A�@ 1'@ b@�@�@�@�;@\)@�@�@��@E�@�T@�h@V@�/@�D@�D@�D@Z@1@S�@"�@�@��@�!@M�@��@��@hs@G�@G�@X@7L@%@��@�9@�@A�@b@�@�@�;@�w@��@l�@;d@
=@�R@v�@E�@@��@@�-@�-@��@��@�h@�h@�@�@p�@`B@`B@O�@�@��@��@�D@I�@9X@(�@�@�@1@��@�@C�@33@�@��@^5@��@�^@��@��@x�@%@�u@Q�@1'@ �@ �@b@�w@l�@\)@;d@�@
=@
=@��@��@$�@�@?}@?}@/@��@�D@z�@Z@I�@I�@I�@I�@�@��@�F@��@dZ@C�@
�@
��@
��@
�\@
M�@
-@	�#@	G�@	�@��@�`@�9@�u@r�@A�@b@�@��@��@|�@\)@K�@;d@+@��@��@�y@ȴ@�+@V@V@5?@@@{@{@@@@�@�@�T@�T@�T@��@�-@`B@��@��@�j@�D@I�@�@�
@t�@S�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B;dB;dB:^B:^B:^B;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB:^B9XB9XB:^B:^B?}BP�BO�BJ�B@�B7LB(�BuB�fB��B�XB��B��B��B�=BjB;dB.B+B&�B�B%B�yB�HB�/B�/B�;B�mB�B�B��B�B�B�fB��B�XB�B��B��B�hB}�B_;B49B�BuB	7BB
�B
�yB
�mB
�5B
ǮB
��B
�-B
��B
�oB
�DB
�B
�B
}�B
w�B
q�B
k�B
dZB
XB
S�B
P�B
I�B
@�B
=qB
6FB
 �B
�B
�B
�B
oB
\B
DB	��B	�B	�B	�B	�yB	�mB	�sB	�#B	�B	�#B	�
B	��B	��B	�}B	�dB	�dB	�wB	�qB	�FB	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�B	q�B	ffB	bNB	`BB	^5B	cTB	XB	O�B	N�B	N�B	W
B	[#B	^5B	[#B	S�B	Q�B	G�B	B�B	=qB	9XB	6FB	1'B	,B	)�B	%�B	"�B	!�B	�B	uB	\B	VB	+B	  B��B��B�B�B�B�B�B�mB�5B�
B�B��B��B��BȴBĜB��B�}B�^B�FB�9B�3B�'B�B�B��B��B��B��B��B��B�oB�\B�JB�=B�7B�B�B�B�B� B� B|�B{�Bz�By�Bw�Bv�Bs�Bo�Bn�Bl�BiyBhsBffBdZBdZB]/BYBT�BQ�BO�BL�BK�BH�BE�BE�B?}B@�B:^B8RB8RB6FB5?B5?B49B33B1'B0!B/B-B,B,B,B,B+B,B+B+B)�B)�B'�B&�B&�B%�B$�B#�B#�B#�B!�B#�B!�B!�B!�B �B!�B �B �B�B"�B!�B"�B"�B"�B!�B!�B$�B%�B&�B&�B&�B(�B)�B,B,B.B/B0!B1'B1'B0!B0!B1'B1'B2-B2-B2-B5?B7LB9XB:^B?}B@�B@�BA�BE�BF�BG�BH�BK�BM�BO�BN�BO�BQ�BR�BR�BS�BT�BT�BXBZB[#B^5BffBiyBjBk�Bk�Bm�Bm�Bl�Bn�Bq�Bt�Bs�Bt�Bw�Bz�B{�B|�B� B�B�B�B�1B�JB�bB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�?B�XB�qB�wB�}BÖBŢBǮBǮBɺB��B��B��B��B�B�B�B�#B�/B�;B�;B�;B�NB�TB�ZB�ZB�ZB�TB�TB�ZB�`B�fB�fB�sB�yB�yB�B�B�B�B�B��B��B��B��B��B	  B	B	%B	+B	+B	1B	DB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	&�B	(�B	+B	-B	/B	7LB	?}B	A�B	D�B	F�B	G�B	H�B	H�B	J�B	J�B	J�B	K�B	L�B	M�B	N�B	Q�B	S�B	YB	\)B	^5B	_;B	`BB	aHB	aHB	cTB	gmB	iyB	iyB	k�B	m�B	n�B	p�B	r�B	t�B	u�B	w�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�=B	�DB	�JB	�PB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�9B	�9B	�9B	�9B	�?B	�LB	�XB	�XB	�^B	�jB	�jB	�jB	�qB	�wB	�}B	��B	ÖB	ŢB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�/B	�5B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�fB	�sB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
\B
\B
bB
hB
hB
hB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
,B
-B
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
2-B
33B
49B
49B
49B
49B
49B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
?}B
?}B
?}B
>wB
>wB
>wB
?}B
?}B
?}B
?}B
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
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
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
P�B
P�B
P�B
P�B
P�B
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
YB
YB
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
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
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
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
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
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B;dB;dB:^B:^B:^B;dB;dB;dB;dB;dB;dB;dB;dB;B;�B<B;B;�B;B<�B>�BF�BW�BS�BM�BD�B@�B6B�B�BՁB��B�"B�B��B��Bq�B=�B/B,�B+B$ZB	�B�B��B��BݘB�;B�RB�B�'B��B�MB��B��BؓB�B��B�B�1B��B��Bc�B6�BB�B
�BGB
��B
�B
��B
�vB
�7B
��B
�TB
��B
��B
�B
��B
��B
HB
y	B
sB
mCB
f2B
X�B
UB
R B
J�B
AUB
?cB
8RB
!|B
5B
CB
B
&B
�B
�B	��B	�B	� B	�"B	�0B	�>B	�B	ۦB	ٴB	�]B	ؓB	�B	� B	��B	��B	��B	��B	�]B	�2B	�GB	��B	�FB	�nB	�B	��B	��B	�]B	��B	�aB	��B	shB	g8B	c B	abB	_�B	e,B	Y1B	P}B	OBB	O\B	XB	\xB	`vB	\B	UMB	T�B	IB	C�B	>(B	:^B	7�B	2B	,�B	+6B	&�B	#�B	#�B	=B	�B	}B	�B		B	�B�VB�XB�aB�B�B�CB�B�yB�;B��B�YB�pB��B� BɆB�SB�uB��B��B��B��B��B��B� B��B��B��B��B��B��B�
B��B�HB�PB��B�=B��B�{B��B��B��B��B}VB|jB{BzxBy	Bx8Bt�Bp�Bo�BmwBj0Bi*BgRBf2Bf�B_;BZ�BV�BS@BQ BN"BMjBJXBG_BG�B@�BB�B;JB9>B9XB6�B5�B6FB5�B4�B2aB1�B0!B-�B,�B,�B,qB,qB+kB,WB+�B+�B+B+B(sB'�B'�B&�B%�B%`B%�B%B# B$�B"�B"�B"�B"B"NB!-B!�B!bB$@B"�B#TB#TB#nB"�B#�B&�B'B(>B'�B(XB*B+QB,�B,�B.}B/�B0�B1�B1vB0�B0oB1�B1�B2�B2�B3MB6`B8RB:*B;�B@4BABA BB[BFBGBH1BI�BL�BN�BPbBO�BP}BR:BS@BS@BTaBUMBU�BX_BZ�B[WB^jBf�Bi�BkBl"Bk�Bm�Bm�Bl�Bo BraBu?BtBuZBxB{0B|B}<B�OB�UB�oB��B��B��B��B��B��B��B��B�
B�B�B�'B�\B�TB��B�LB�$B��B��B�=B�CB��B�B�B�*B��B��B�4B�B�B��B��B�=B�HB�@B�FB�MBևB�KB�7B�qBݲB�VB�pB��B��B�B�B�B��B��B�B��B��B�B�B�B�B�B�B��B��B�B�3B�B�B�B�RB�<B	 OB	oB	tB	_B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	# B	#�B	%B	'B	)B	+B	-)B	.�B	72B	?�B	A�B	D�B	F�B	G�B	IB	IB	J�B	J�B	J�B	LB	MB	N"B	O\B	R:B	TaB	Y1B	\CB	^OB	_pB	`\B	abB	a�B	c�B	g�B	i�B	i�B	k�B	m�B	n�B	p�B	r�B	t�B	u�B	xB	|6B	~�B	�B	�GB	�-B	�{B	�3B	�B	�YB	�fB	�lB	�XB	�XB	�xB	�dB	��B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�&B	�,B	�8B	�B	�B	�0B	�6B	�QB	�wB	�;B	�[B	�GB	�aB	�3B	�3B	�nB	�TB	�TB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�4B	� B	�[B	�?B	�$B	�EB	�eB	�kB	�7B	�QB	�kB	�WB	�WB	�IB	�/B	�OB	�jB	�VB	ߊB	�|B	�hB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�$B	��B	��B	��B	��B	�0B	�6B	�VB
 4B
 B
;B
'B
AB
GB
{B
mB
9B
YB
_B
�B
�B
	lB

XB

rB
xB
^B
^B
dB
dB
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
 �B
"B
"�B
"�B
"�B
"�B
#B
"�B
$B
#�B
#�B
$�B
$�B
$�B
&B
%�B
&B
'B
($B
($B
)*B
)*B
*0B
*0B
+B
+B
,=B
,=B
,"B
,"B
-CB
.IB
./B
.B
.IB
/5B
/5B
/5B
0;B
0;B
0oB
1vB
1AB
2|B
3hB
4nB
4TB
4nB
4TB
4TB
5ZB
5tB
6zB
7fB
7fB
7fB
7fB
7fB
7fB
7�B
7�B
7fB
8�B
8lB
8lB
8�B
8lB
8lB
8�B
9�B
9�B
9�B
9rB
9rB
9�B
9rB
9rB
9�B
:xB
:xB
;B
;dB
;B
;B
<�B
=�B
=�B
=�B
=�B
?�B
?�B
?�B
>�B
>�B
>�B
?�B
?�B
?}B
?}B
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
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
MB
M�B
NB
NB
NB
M�B
OB
PB
PB
O�B
O�B
PB
P.B
PB
QB
QB
QB
Q B
Q B
Q B
QB
QB
RB
Q�B
Q�B
R B
RB
RB
S&B
SB
SB
S&B
T,B
S�B
TB
T,B
TB
T,B
T,B
U2B
U2B
UB
V9B
VB
VB
VB
W$B
W
B
W
B
W$B
W
B
W
B
W$B
W
B
W
B
W$B
W
B
W$B
W?B
XEB
X+B
X+B
XEB
YB
Y1B
YB
Y1B
Y1B
Y1B
YeB
YKB
Z7B
ZQB
ZQB
ZkB
[WB
[WB
[#B
[=B
[WB
[qB
\xB
]IB
]IB
^5B
^OB
^jB
^OB
^jB
_VB
_VB
_pB
_;B
_VB
_pB
_pB
`vB
`vB
a|B
abB
abB
abB
bhB
bhB
bhB
bhB
bNB
bNB
bNB
bhB
bhB
cnB
c�B
cnB
cnB
dtB
d�B
dtB
d�B
e�B
ezB
e�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
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
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Q�<7�4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609040038532016090400385320160904003853201806221213162018062212131620180622121316201804050405492018040504054920180405040549  JA  ARFMdecpA19c                                                                20160831093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160831003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160831003514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160831003514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160831003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160831003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160831003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160831003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160831003515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160831003516                      G�O�G�O�G�O�                JA  ARUP                                                                        20160831011955                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160831153309  CV  JULD            G�O�G�O�F�9�                JM  ARCAJMQC2.0                                                                 20160903153853  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160903153853  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190549  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031316  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                