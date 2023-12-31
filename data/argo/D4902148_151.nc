CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-10-05T15:35:47Z creation;2018-10-05T15:35:50Z conversion to V3.1;2019-12-18T07:19:47Z update;2022-11-21T05:30:03Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181005153547  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_151                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @؆�<M^�1   @؆�F)�@<������d� ѷ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @���A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0ffB8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�
@.{@z�H@�=q@�p�A�A>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)BG�B�B�B�B'�B0{B7�B?�BG�BOG�BW�B_�Bg�Bo�Bw�B�B��
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
CC�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP�{DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_t{D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��=D�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�J=D�]q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�A�A�C�A�C�A�E�A�E�A�C�A�E�A�G�A�I�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�O�A�M�A�7LA˛�A�JAɅA�-A�|�A�-A��+A��^A��A��A��A��A��A�dZA���A�33A��wA�JA�p�A�ĜA��PA��+A�z�A��mA� �A�ffA���A��A��A���A�A�A�M�A���A��TA�{A��A��hA�Q�A���A��FA��/A�$�A���A���A�p�A�"�A�"�A�/A��#A�jA�~�A���A�`BA�oA���A�A�A�/A�\)A�+AdZAVA}�-Az�AzVAzAxZAw
=Au�TAtbNAr�Ap�uAo��An��AmVAl$�Ak�7Aj(�AhA�Af�uAe\)AdAb��Ab��AbQ�Aa��Aa&�A`�/A`n�A`1'A_��A^ĜA^I�A]�FA]p�A\�A[�AZ��AZr�AY��AYx�AX��AX�9AWS�AV�jAU��AUK�AT9XARr�AQ�^AQ%AOp�ANv�ANI�AN1'AN1AM�mAM�ALr�AJbNAJ-AJ  AI�hAH�HAF�AE��AE�AD��ADQ�AC��ACG�ABv�A@A�A>$�A=�wA=�A=G�A=�A=%A=A<��A<�`A<��A<n�A<-A;�A;S�A9ƨA7�A6��A5�;A5�A4�yA4E�A3�A3x�A1�PA0�A/�wA/�A/G�A. �A-`BA,�yA,~�A+�A*�9A*n�A)�FA)�A(A�A(bA(A'�;A'A'A%��A$��A#;dA"��A"��A"�\A"5?A!�A!��A!l�A!33AC�A�+AZA9XA�A�At�AVA"�A�7AC�A��A9XAA�A~�A�AO�A�HAjAbA�TA��A7LAȴA(�AoA�uA5?A�;A�A�^A^5A
��A
JA	��A	;dA�A�\A�AA��A&�A9XA\)AoA�`A��A�
A ��@��w@�v�@�`B@���@��@�@��@�  @�dZ@���@�F@@�@�&�@�1@�hs@� �@�@�M�@��@�9@��@��H@�@�%@�A�@�S�@���@�~�@��@�p�@�X@���@��@��y@���@�^5@���@׾w@�-@�bN@���@щ7@�1'@�
=@·+@�@�x�@̛�@�(�@˅@ȴ9@�J@�=q@�X@��;@��@�V@�`B@���@���@�ƨ@�S�@���@�{@�7L@�j@�1@�ƨ@���@�|�@�;d@�
=@�=q@�?}@�(�@�n�@�V@���@�A�@���@�V@���@�9X@�S�@��@���@���@��h@���@��T@��@��@��9@��@�1@��@��+@�^5@�-@��^@���@���@���@�dZ@��+@��@�`B@�V@��@���@�C�@�+@�v�@��@��-@��@��`@��/@���@�Ĝ@�j@�Q�@�A�@�(�@��@���@�"�@��!@���@���@���@��
@�|�@�S�@�+@�o@���@���@��T@��7@�`B@���@�I�@�  @�ƨ@��@���@��P@�K�@�C�@��@���@�n�@���@���@�&�@���@���@��@�bN@�b@���@�;d@�33@�33@�+@��@�^5@�{@���@�&�@���@��@��/@���@���@��j@�9X@��@��@�\)@��@�@�
=@�o@��\@���@���@���@�(�@��@�l�@��@�M�@��@��@��@��T@��@��9@���@�j@�Z@�Z@�(�@|�@~E�@}`B@}/@|��@{"�@z��@z�!@zn�@z�@y�@y��@yhs@x��@x�@w\)@w+@w
=@w
=@v�y@u@uV@t��@tj@t9X@t1@sƨ@s��@r�H@q��@qhs@q�@pbN@p  @o�w@o�P@o;d@o�@n��@n��@nE�@n{@m�@l��@l��@l�D@lz�@lz�@lj@lj@k��@k@j��@j��@j��@jn�@i�#@ihs@iX@iG�@h��@h1'@g�@gl�@g\)@g\)@gK�@g�@f�@f�@f�@f�@f�@f�@f�y@f�@f�R@fff@f5?@f{@f@e��@e�h@eV@eV@e�@eV@d��@d�j@c�@b��@b�\@b=q@a��@aG�@`�u@`�@`bN@_�;@_�@^�y@^��@^E�@]�@]�-@]�@]�-@]�@]V@\�@\�D@\�@[ƨ@[�@[S�@[o@Z��@Z~�@ZM�@YX@Y%@Xr�@X �@X �@W��@W;d@WK�@W+@V�R@VV@V{@U�-@UV@T�@T�@T�/@T��@T��@T��@S�m@R~�@Q�@Q�#@Q�^@Q��@Q��@Q�7@Q�7@Qx�@Q7L@P�`@PbN@Pb@O��@O�w@O�P@O\)@N��@N��@Nv�@N$�@N{@M�@M�-@MO�@L�/@L��@L�@K��@KdZ@Ko@J�!@I��@I��@Ihs@I�@Hr�@HA�@H �@Hb@G��@G��@G��@G�@G�P@G|�@G+@FE�@E�-@EO�@D��@DI�@D1@C��@C��@Ct�@C33@B��@B=q@A�@A��@A%@@Ĝ@@�u@@ �@@  @?��@?��@?\)@>��@>�R@>�+@>V@>V@>{@=��@=`B@=?}@=�@<��@<�/@<�@<Z@<�@;�m@;dZ@;o@:��@:n�@:M�@:M�@:=q@9��@9��@9X@9&�@9�@8��@8bN@8A�@8 �@7�;@7�@7�P@7;d@6ff@65?@6@5�@5�T@5@5��@5��@5`B@5/@5�@4�@49X@4(�@41@3�m@3��@3t�@3C�@2�@2�\@2~�@2M�@2J@1x�@1�@0��@0�u@0Q�@0 �@/��@/l�@/�@.��@.��@.V@.@.@-�-@-p�@-V@,�j@,z�@,1@+�m@+��@+S�@+o@*�@*��@*�\@*n�@*^5@)G�@(��@(�`@(��@(��@(�9@(��@(�u@(�u@(r�@(r�@(bN@(  @';d@&�+@&V@&5?@%��@$�j@$�@$�D@$z�@$Z@$Z@$Z@$I�@#��@#��@#t�@#S�@#@"�!@"n�@"�@!��@!�^@!x�@!x�@!hs@!hs@!X@!X@!X@!X@!X@!X@!X@!X@!G�@!&�@!�@ ��@ bN@�;@;d@v�@E�@{@@@@�@�T@��@�/@I�@�F@dZ@"�@o@@�@��@�\@^5@=q@�@x�@&�@%@%@��@�`@�`@�@�;@�@�@�@�@��@|�@\)@�y@�R@�+@ff@E�@$�@��@��@��@��@��@�T@�h@O�@V@�/@��@��@�j@�j@�@��@�D@��@z�@j@Z@9X@�@��@�
@33@o@@�H@^5@=q@-@�@�@��@��@��@��@X@7L@7L@&�@��@��@�9@��@�@ �@�w@�@|�@;d@+@
=@��@��@��@��@�y@�@�R@��@��@v�@ff@V@E�@5?@$�@{@@/@��@�@�@�D@��@��@dZ@S�@"�@
�@
��@
�!@
�\@
�\@
~�@
n�@
^5@
M�@
M�@
M�@
=q@
-@
-@
-@
�@
J@
J@	��@	�#@	�^@	�7@	�7@	�7@	X@	�@�`@Ĝ@��@�u@�u@�u@�@bN@1'@��@��@�P@�P@|�@\)@K�@;d@+@�@��@��@�y@�@�R@@@�h@p�@`B@/@��@�@�@j@9X@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�A�A�C�A�C�A�E�A�E�A�C�A�E�A�G�A�I�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�O�A�M�A�7LA˛�A�JAɅA�-A�|�A�-A��+A��^A��A��A��A��A��A�dZA���A�33A��wA�JA�p�A�ĜA��PA��+A�z�A��mA� �A�ffA���A��A��A���A�A�A�M�A���A��TA�{A��A��hA�Q�A���A��FA��/A�$�A���A���A�p�A�"�A�"�A�/A��#A�jA�~�A���A�`BA�oA���A�A�A�/A�\)A�+AdZAVA}�-Az�AzVAzAxZAw
=Au�TAtbNAr�Ap�uAo��An��AmVAl$�Ak�7Aj(�AhA�Af�uAe\)AdAb��Ab��AbQ�Aa��Aa&�A`�/A`n�A`1'A_��A^ĜA^I�A]�FA]p�A\�A[�AZ��AZr�AY��AYx�AX��AX�9AWS�AV�jAU��AUK�AT9XARr�AQ�^AQ%AOp�ANv�ANI�AN1'AN1AM�mAM�ALr�AJbNAJ-AJ  AI�hAH�HAF�AE��AE�AD��ADQ�AC��ACG�ABv�A@A�A>$�A=�wA=�A=G�A=�A=%A=A<��A<�`A<��A<n�A<-A;�A;S�A9ƨA7�A6��A5�;A5�A4�yA4E�A3�A3x�A1�PA0�A/�wA/�A/G�A. �A-`BA,�yA,~�A+�A*�9A*n�A)�FA)�A(A�A(bA(A'�;A'A'A%��A$��A#;dA"��A"��A"�\A"5?A!�A!��A!l�A!33AC�A�+AZA9XA�A�At�AVA"�A�7AC�A��A9XAA�A~�A�AO�A�HAjAbA�TA��A7LAȴA(�AoA�uA5?A�;A�A�^A^5A
��A
JA	��A	;dA�A�\A�AA��A&�A9XA\)AoA�`A��A�
A ��@��w@�v�@�`B@���@��@�@��@�  @�dZ@���@�F@@�@�&�@�1@�hs@� �@�@�M�@��@�9@��@��H@�@�%@�A�@�S�@���@�~�@��@�p�@�X@���@��@��y@���@�^5@���@׾w@�-@�bN@���@щ7@�1'@�
=@·+@�@�x�@̛�@�(�@˅@ȴ9@�J@�=q@�X@��;@��@�V@�`B@���@���@�ƨ@�S�@���@�{@�7L@�j@�1@�ƨ@���@�|�@�;d@�
=@�=q@�?}@�(�@�n�@�V@���@�A�@���@�V@���@�9X@�S�@��@���@���@��h@���@��T@��@��@��9@��@�1@��@��+@�^5@�-@��^@���@���@���@�dZ@��+@��@�`B@�V@��@���@�C�@�+@�v�@��@��-@��@��`@��/@���@�Ĝ@�j@�Q�@�A�@�(�@��@���@�"�@��!@���@���@���@��
@�|�@�S�@�+@�o@���@���@��T@��7@�`B@���@�I�@�  @�ƨ@��@���@��P@�K�@�C�@��@���@�n�@���@���@�&�@���@���@��@�bN@�b@���@�;d@�33@�33@�+@��@�^5@�{@���@�&�@���@��@��/@���@���@��j@�9X@��@��@�\)@��@�@�
=@�o@��\@���@���@���@�(�@��@�l�@��@�M�@��@��@��@��T@��@��9@���@�j@�Z@�Z@�(�@|�@~E�@}`B@}/@|��@{"�@z��@z�!@zn�@z�@y�@y��@yhs@x��@x�@w\)@w+@w
=@w
=@v�y@u@uV@t��@tj@t9X@t1@sƨ@s��@r�H@q��@qhs@q�@pbN@p  @o�w@o�P@o;d@o�@n��@n��@nE�@n{@m�@l��@l��@l�D@lz�@lz�@lj@lj@k��@k@j��@j��@j��@jn�@i�#@ihs@iX@iG�@h��@h1'@g�@gl�@g\)@g\)@gK�@g�@f�@f�@f�@f�@f�@f�@f�y@f�@f�R@fff@f5?@f{@f@e��@e�h@eV@eV@e�@eV@d��@d�j@c�@b��@b�\@b=q@a��@aG�@`�u@`�@`bN@_�;@_�@^�y@^��@^E�@]�@]�-@]�@]�-@]�@]V@\�@\�D@\�@[ƨ@[�@[S�@[o@Z��@Z~�@ZM�@YX@Y%@Xr�@X �@X �@W��@W;d@WK�@W+@V�R@VV@V{@U�-@UV@T�@T�@T�/@T��@T��@T��@S�m@R~�@Q�@Q�#@Q�^@Q��@Q��@Q�7@Q�7@Qx�@Q7L@P�`@PbN@Pb@O��@O�w@O�P@O\)@N��@N��@Nv�@N$�@N{@M�@M�-@MO�@L�/@L��@L�@K��@KdZ@Ko@J�!@I��@I��@Ihs@I�@Hr�@HA�@H �@Hb@G��@G��@G��@G�@G�P@G|�@G+@FE�@E�-@EO�@D��@DI�@D1@C��@C��@Ct�@C33@B��@B=q@A�@A��@A%@@Ĝ@@�u@@ �@@  @?��@?��@?\)@>��@>�R@>�+@>V@>V@>{@=��@=`B@=?}@=�@<��@<�/@<�@<Z@<�@;�m@;dZ@;o@:��@:n�@:M�@:M�@:=q@9��@9��@9X@9&�@9�@8��@8bN@8A�@8 �@7�;@7�@7�P@7;d@6ff@65?@6@5�@5�T@5@5��@5��@5`B@5/@5�@4�@49X@4(�@41@3�m@3��@3t�@3C�@2�@2�\@2~�@2M�@2J@1x�@1�@0��@0�u@0Q�@0 �@/��@/l�@/�@.��@.��@.V@.@.@-�-@-p�@-V@,�j@,z�@,1@+�m@+��@+S�@+o@*�@*��@*�\@*n�@*^5@)G�@(��@(�`@(��@(��@(�9@(��@(�u@(�u@(r�@(r�@(bN@(  @';d@&�+@&V@&5?@%��@$�j@$�@$�D@$z�@$Z@$Z@$Z@$I�@#��@#��@#t�@#S�@#@"�!@"n�@"�@!��@!�^@!x�@!x�@!hs@!hs@!X@!X@!X@!X@!X@!X@!X@!X@!G�@!&�@!�@ ��@ bN@�;@;d@v�@E�@{@@@@�@�T@��@�/@I�@�F@dZ@"�@o@@�@��@�\@^5@=q@�@x�@&�@%@%@��@�`@�`@�@�;@�@�@�@�@��@|�@\)@�y@�R@�+@ff@E�@$�@��@��@��@��@��@�T@�h@O�@V@�/@��@��@�j@�j@�@��@�D@��@z�@j@Z@9X@�@��@�
@33@o@@�H@^5@=q@-@�@�@��@��@��@��@X@7L@7L@&�@��@��@�9@��@�@ �@�w@�@|�@;d@+@
=@��@��@��@��@�y@�@�R@��@��@v�@ff@V@E�@5?@$�@{@@/@��@�@�@�D@��@��@dZ@S�@"�@
�@
��@
�!@
�\@
�\@
~�@
n�@
^5@
M�@
M�@
M�@
=q@
-@
-@
-@
�@
J@
J@	��@	�#@	�^@	�7@	�7@	�7@	X@	�@�`@Ĝ@��@�u@�u@�u@�@bN@1'@��@��@�P@�P@|�@\)@K�@;d@+@�@��@��@�y@�@�R@@@�h@p�@`B@/@��@�@�@j@9X@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�FB	7B"�B�B+B��BƨB��B�jB�FB�'B��B��B�bB�=B�Bw�Bo�BcTB]/BYBJ�B:^B-B"�B�B{B
=BB��B��B�mB��BǮB�dB�B��B�uB�%Bl�B_;BQ�B<jB1'B+B$�B�B
=BB
��B
�B
�fB
�NB
�)B
��B
��B
�'B
��B
�=B
�B
�B
w�B
jB
gmB
cTB
W
B
M�B
E�B
:^B
33B
(�B
"�B
�B
oB
DB
%B	��B	�B	�yB	�TB	�)B	�
B	��B	��B	��B	ɺB	ǮB	ĜB	B	�}B	�dB	�XB	�LB	�?B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�DB	�%B	~�B	z�B	v�B	p�B	m�B	l�B	l�B	k�B	jB	gmB	aHB	ZB	YB	W
B	R�B	L�B	A�B	9XB	6FB	33B	1'B	.B	+B	$�B	�B	uB	oB	hB	bB	\B	\B	\B	\B	VB	PB	PB	DB		7B	B��B��B�B�B�B�sB�fB�ZB�BB�B�B��B��B��B��BǮBŢBÖB�}B�jB�dB�RB�?B�3B�3B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�\B�VB�PB�JB�=B�%B�B~�B}�B|�Bz�By�Bw�Bu�Bs�Br�Bp�Bo�Bo�Bn�Bm�Bk�BjBgmBe`BdZBcTBaHB^5B[#BVBS�BR�BQ�BP�BO�BN�BM�BK�BH�BE�BC�BB�BA�B@�B?}B<jB:^B9XB8RB7LB6FB49B1'B0!B/B-B,B+B)�B)�B(�B&�B%�B%�B%�B&�B%�B&�B&�B&�B%�B$�B#�B#�B#�B#�B#�B$�B$�B$�B$�B%�B(�B(�B'�B'�B(�B(�B(�B(�B(�B+B,B-B-B-B,B+B+B,B1'B0!B.B,B-B2-B49B49B8RB8RB:^B;dB;dB=qB>wB>wB?}B?}B@�B?}B@�BB�BB�BC�BD�BD�BD�BI�BL�BM�BW
BXBXBYBYB[#B^5BgmBjBjBk�Bk�Bm�Br�Bt�Bt�Bt�Bv�Bv�By�B}�B~�B�B�B�1B�7B�=B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�3B�9B�?B�FB�XB�wB��B��BĜBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�)B�)B�5B�BB�NB�TB�ZB�fB�mB�yB�B�B�B�B�B�B�B�B�B��B��B	B	B	B		7B	DB	DB	\B	{B	�B	�B	�B	�B	�B	 �B	$�B	%�B	'�B	'�B	'�B	/B	33B	33B	5?B	5?B	5?B	6FB	9XB	=qB	@�B	A�B	D�B	K�B	M�B	M�B	N�B	O�B	P�B	Q�B	R�B	S�B	T�B	YB	ZB	ZB	ZB	[#B	_;B	bNB	cTB	e`B	ffB	gmB	hsB	iyB	jB	s�B	u�B	v�B	x�B	y�B	{�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�7B	�DB	�DB	�DB	�JB	�JB	�DB	�PB	�bB	�oB	�uB	�{B	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�?B	�LB	�^B	�dB	�^B	�dB	�qB	�wB	�wB	��B	��B	��B	B	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
	7B

=B
DB
JB
PB
PB
VB
VB
VB
\B
\B
hB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
'�B
(�B
(�B
(�B
)�B
+B
)�B
+B
+B
+B
+B
,B
,B
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
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
<jB
<jB
=qB
>wB
>wB
>wB
?}B
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
C�B
D�B
D�B
E�B
F�B
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
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
N�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
gmB
gmB
gmB
gmB
gmB
hsB
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
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�jB��B�rB
�B&�B"B,B�-BΊB�B��B��B��B��B�	B�oB��B�3By�Br�Bd�B^�B[�BOB=�B.�B$tB	B�B�BaB�<B�8B�eB�sBɺB��B��B��B��B��Bn�BabBT�B>BB2-B,=B'B�B^B�B
�B
�|B
�RB
�TB
�B
�?B
ĜB
��B
�B
�^B
�B
�B
z^B
kkB
hsB
e,B
X�B
OvB
G�B
<jB
5�B
*0B
$B
�B
�B
dB
B	�HB	��B	�B	��B	�IB	רB	ՁB	ҽB	�jB	�#B	�KB	�B	�aB	��B	�6B	�B	�B	�+B	��B	��B	��B	��B	�`B	��B	��B	�!B	�yB	�{B	�}B	��B	�B	�B	|B	x�B	q�B	m�B	l�B	l�B	k�B	kQB	iB	cTB	Z�B	YB	W�B	TFB	N�B	CB	:*B	6�B	3�B	1�B	/5B	,�B	'�B	�B	,B	�B	�B	�B	vB	vB	vB	�B	�B	�B	�B	�B	
�B	_B�HB�B��B�IB�QB�DB�8B�B�hB�7B�YB҉BѝB�HB��B�fB�tB��B��B�B�jB�XB�+B��B�hB��B��B�oB��B�sB�FB�HB�B�;B�OB�/B�)B�#B��B��B�oB��B��B��B��B�jB��B��B��B�B~�B}�B{�Bz�Bx�Bv�Bt�BshBqvBp;BpBoBnIBlWBk�Bh�Bf2Bd�Bd&Bb�B`BB]/BW�BT�BS�BR�BQhBP�BO�BN�BMPBJ�BF�BD�BCBBBAUB@�B>B;B:DB9$B8B72B6B3hB1[B/�B.IB-]B+�B*�B*�B*B(�B&�B&�B&�B'�B&�B'�B'�B'�B&�B%zB$tB$@B$ZB$ZB$ZB%FB%�B%`B%,B&LB)�B*B)B)*B*KB*B)�B)�B)�B+�B,�B-�B-�B-�B,�B,�B,qB,=B1�B1AB.�B,�B-�B3hB5�B5�B8�B9	B:�B<B<B=�B>�B>�B?�B?�B@�B@4BAUBC{BC�BD�BEBE9BE�BJXBMjBOBW�BX_BXyBY�BZB\)B_�Bg�Bj�Bj�Bk�BlBncBsBuBu%Bu%BwBw�Bz�B~]B�B��B��B��B��B��B��B��B�B��B��B�#B��B��B��B��B�B��B��B�B�B�;B�'B�:B�FB��B��B��B��B�aB�hB��B�tB��B��B��B��B�B�B�	B��B��B�B�B�B��B�<B�.B� B�[B�MBևB�B�kB�WB�xB�xBބB�vB�B�nB�B�B��B��B��B��B��B��B��B��B��B��B�B�8B�B	UB	GB	9B		lB	^B	�B	�B	B	�B	B	/B	B	;B	!HB	%B	&2B	(
B	($B	(�B	/iB	3hB	3�B	5tB	5ZB	5tB	6�B	9�B	=�B	@�B	A�B	E9B	K�B	M�B	M�B	OB	PB	QB	R B	S&B	TaB	UgB	Y1B	ZQB	ZQB	ZQB	[�B	_pB	b�B	c�B	ezB	f�B	g�B	h�B	i�B	kB	s�B	u�B	wB	y	B	y�B	|B	}B	~B	~B	~(B	�OB	�'B	�[B	�gB	�RB	�^B	�^B	�xB	�~B	�~B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�$B	��B	�B	�B	�0B	�QB	�kB	�OB	�GB	�hB	��B	��B	��B	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	B	żB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�4B	�B	�FB	�B	�,B	�,B	�B	��B	�2B	�SB	�_B	�7B	�]B	�dB	�HB	�HB	�|B	�bB	�hB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�(B	�.B	�HB
;B
AB
[B
aB
MB
9B
9B
SB
%B
?B
?B
?B
YB
YB
zB
	�B

rB
xB
~B
jB
�B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
!�B
"�B
#B
#�B
$B
#�B
#�B
%B
$�B
%,B
&LB
'B
($B
(
B
(
B
(
B
($B
(�B
(
B
)*B
)*B
)*B
*0B
+B
*B
+B
+B
+B
+6B
,=B
,"B
-)B
-)B
./B
.cB
/OB
/5B
/OB
0;B
0UB
0oB
1AB
2aB
2aB
3MB
3MB
3MB
49B
4TB
4TB
4nB
5ZB
5ZB
6zB
6zB
6`B
6`B
7�B
7fB
7�B
8�B
9rB
9rB
9�B
;�B
;dB
;dB
<�B
<�B
<�B
<jB
<jB
<�B
<jB
<�B
<�B
<�B
=�B
>�B
>�B
>�B
?�B
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
C�B
D�B
D�B
E�B
F�B
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
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
J	B
J	B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L0B
MB
NB
N�B
OB
Q B
Q�B
R B
RB
RB
S&B
S&B
SB
R:B
R B
SB
R�B
SB
R�B
S&B
S@B
TFB
UB
T�B
T�B
VB
VB
VB
W$B
W?B
W$B
W$B
X+B
X+B
XEB
XEB
YB
Y1B
Y1B
Y1B
YB
Y1B
ZQB
Z7B
Z7B
[=B
[#B
[#B
[=B
[=B
[#B
[=B
[#B
[=B
[=B
[#B
[WB
\CB
\CB
\CB
\xB
]dB
]dB
^OB
^�B
^OB
_;B
_VB
_;B
_VB
_;B
_VB
_VB
`\B
`vB
`BB
`\B
`vB
`vB
a|B
abB
a|B
a|B
abB
bNB
b�B
b�B
cTB
c�B
cTB
cnB
dZB
dZB
dZB
dtB
dtB
dtB
dZB
d�B
e`B
e`B
e`B
ezB
ezB
ezB
e�B
e�B
g�B
gmB
g�B
g�B
g�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
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
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Nq�<b�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810170035322018101700353220181017003532202211182136292022111821362920221118213629201810180020002018101800200020181018002000  JA  ARFMdecpA19c                                                                20181006003529  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181005153547  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181005153549  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181005153549  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181005153550  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181005153550  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181005153550  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181005153550  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181005153550  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181005153550                      G�O�G�O�G�O�                JA  ARUP                                                                        20181005155510                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181005153517  CV  JULD            G�O�G�O�F�4�                JM  ARCAJMQC2.0                                                                 20181016153532  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181016153532  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181017152000  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123629  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                