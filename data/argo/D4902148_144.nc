CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-07-27T15:35:47Z creation;2018-07-27T15:35:50Z conversion to V3.1;2019-12-18T07:21:18Z update;2022-11-21T05:30:24Z update;     
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
resolution        =���   axis      Z        x  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  Mx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  �0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180727153547  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_144                     2C  Dd9ZNAVIS_A                         0397                            ARGO 011514                     863 @�u"�ޠ 1   @�u#�� @<�hr� ��d9Z����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�
@z�H@�p�@�p�A�A>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_G�BgG�BoG�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DM�HDM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��=D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z=D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǺ=D��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111-A�M�A�S�A�O�A�Q�A�K�A�33A��TAƸRAƮAƣ�Aƛ�AƍPA�t�A�=qA�-A��A�
=A��TAť�A�|�A�dZA�I�A�-A���A�v�A�jA�C�A�ĜA��9A��A�(�A���A�-A�9XA��A��HA��A��HA��A���A��/A�5?A���A�hsA�\)A�x�A��;A�A�A���A��!A��PA��hA�  A��\A�-A���A�?}A�bNA�bNA�ĜA��RA��/A��A��!A�A�A�ZA�33A���A�(�A� �A�%A��A��A��A�bA~�A}��A|��Az~�Aw�mAu`BAtQ�As�
Asl�Ar�\Ap��Ao
=Am�Ak��Aj{Ah��Ag�;Agt�Ag/Af1Ac�mAc
=AbVAb1'Aa�Aa%A`9XA_O�A^{A\��A\A[K�AZ�AZ1AYdZAX^5AW�AW�AU��ATĜATZATA�ATbAS�^ASXAS+AR�/ARQ�AQ�APz�AOC�AN1AL�HAK+AJffAIdZAH~�AG��AF�jAE��AE�AEADr�AC��AB�9AA�PA@�9A>�jA=��A<�/A;�PA;oA:��A:E�A9K�A8z�A7�mA7K�A6��A6jA6M�A5�^A4��A3�A2��A2��A1�wA1l�A133A0��A0�yA0�9A0M�A/�A/�
A/��A/XA/+A.��A.$�A,�\A+�mA+�
A+�A+�A*5?A(��A'��A'XA&ȴA&9XA%��A%��A$��A#�
A#G�A"�A"�HA"�jA"$�A!�A!XA bAp�AO�A"�AM�A�yA�+A9XAXAM�AG�A1'AXA%A��A�uAffA(�A�A�A�
AhsA�`Az�AJAƨA�hA�!AbA`BA��A�!AQ�A`BA~�AbNA
��A	�FA	dZA	"�A��AE�A��AK�A��A �A�A��A��A�`Az�AA
=A ff@�;d@��@��T@�%@��w@�-@��@���@��@�t�@�"�@�\@�/@�F@��@�Ĝ@�1'@�ƨ@���@�x�@�S�@�@�7L@�9@�Q�@��@��;@㕁@�o@��H@�ȴ@◍@��@�`B@�  @޸R@ۅ@�ff@�p�@���@֟�@��@���@�G�@�bN@�l�@��@̣�@˝�@�v�@Ɂ@�j@ǅ@�
=@ư!@���@ŉ7@�x�@���@�1@�~�@��/@�bN@�I�@�1@��F@��@�|�@�x�@��9@�9X@�;d@�~�@� �@�=q@�Ĝ@�bN@�9X@�  @���@���@���@�%@�9X@�b@��
@�t�@�dZ@��R@���@�`B@���@��
@��@�@�ȴ@�~�@�E�@�z�@�l�@���@��@���@���@�hs@�O�@�G�@�7L@��@���@���@�|�@��R@�=q@�p�@�9X@�|�@�C�@��@�=q@��-@�x�@�p�@�p�@�`B@�?}@�%@��@���@�I�@�1@��F@�|�@���@�v�@�{@���@�X@��@��@�t�@�
=@���@�n�@��#@��@�&�@��@�1@���@�"�@�~�@�M�@�J@��T@��7@���@��u@�bN@�I�@���@�ȴ@��@���@�X@��@�%@��@��/@��9@�(�@���@�t�@��@���@��R@���@��+@�n�@�ff@�M�@��#@��h@��@���@�K�@��H@�5?@��#@��^@��h@��@��j@���@��u@��@�r�@�b@K�@~�y@~V@}��@}/@|��@|��@|��@|��@{�@z�@y��@y�@x��@xr�@xA�@w�@wl�@w+@v�R@v��@vff@vV@vE�@v$�@u@up�@uO�@t��@t(�@sƨ@s�F@s��@s�@st�@sdZ@so@r�@q�7@n��@n�+@m�@m�-@m�-@m`B@l��@lj@k��@ko@j��@jM�@jJ@i�#@i��@iG�@h�`@hĜ@h�u@hr�@hA�@h1'@hr�@h1'@f��@e��@eO�@e?}@dZ@c�@cdZ@b��@co@a�^@a��@ax�@a�@`��@`�u@`1'@_�@^�y@^E�@^@]�h@]?}@]V@\�@\�@\�D@\1@[�@[C�@[@Z�H@Z��@Zn�@Y�@Y�^@Y��@Y��@Y��@Yhs@Y�@X�`@X��@X��@XQ�@Xb@W�w@WK�@WK�@V��@V�@V��@V5?@V{@V{@V{@U@U`B@U?}@U/@T�/@T��@T�j@T�@T�D@T(�@S�m@S��@R�H@RM�@Q�#@Qx�@Q&�@Q�@Q%@P��@P��@Pr�@Pb@O�P@Ol�@O+@O�@O�@N��@N��@NV@M@M`B@M�@L�/@L�j@L�D@LI�@L1@K��@KC�@J�@J�!@J�\@Jn�@Jn�@JM�@J=q@J=q@I�#@I�7@I%@H�9@HQ�@H  @G�w@G��@G+@G
=@F�y@Fȴ@F�R@F�+@F$�@E��@E�@EO�@EV@D��@D�j@D9X@C�
@C�F@C�F@C��@C��@C��@C��@Ct�@Co@B�H@B��@B��@Bn�@BM�@BJ@A�^@AG�@AG�@A7L@A%@@��@@Ĝ@@bN@?�@?��@?�@?��@?��@?��@?��@?�P@?+@>ȴ@>5?@=�T@=��@=@=@=�h@=`B@=`B@=O�@=?}@=�@<��@<��@<�@<�@<9X@;�@;dZ@;dZ@;S�@;C�@;"�@;@:��@:�!@:�\@:~�@:n�@:^5@:-@9�7@9%@8��@8�`@8��@8��@8�9@8��@8�u@8bN@8 �@7��@7|�@7K�@6��@6�y@6�@6��@6ff@5�T@5p�@5V@4�j@4(�@3C�@2�H@2��@2n�@2=q@1�#@1�7@17L@0��@0�@0b@/\)@/
=@.ȴ@-��@-V@,�/@,j@,�@+ƨ@+"�@+@*�!@*n�@*-@*J@)��@)�#@)��@)�7@)G�@(��@(r�@(r�@(A�@( �@( �@(  @'|�@&��@&v�@%�T@%@%�-@%p�@%�@$��@$��@$I�@$1@#�m@#ƨ@#�F@#�F@#S�@"�!@"M�@"=q@"=q@"=q@"=q@"=q@"=q@"-@"�@!��@!7L@ �`@ Ĝ@ Ĝ@ �9@ �u@ �@ r�@ Q�@ b@�w@�P@\)@�y@ȴ@�R@�R@�R@�R@��@��@��@��@��@��@�+@v�@ff@V@E�@�@@O�@��@�j@��@9X@��@��@ƨ@�@S�@"�@@��@~�@-@�#@��@X@Ĝ@��@�@Q�@�@l�@��@�@�R@�+@ff@ff@ff@E�@5?@@�@�T@��@p�@�@�@z�@I�@I�@(�@�F@��@dZ@S�@o@~�@n�@~�@�\@�\@J@x�@7L@&�@%@%@��@��@��@bN@ �@�@�;@��@�@�@�P@l�@
=@�R@ff@5?@�@��@�h@p�@`B@�@V@�@�/@�j@�@��@�D@z�@z�@j@�@��@dZ@C�@o@
�H@
=q@	��@	�#@	��@	x�@	7L@�9@Q�@�;@��@�w@�@�w@�w@�w@��@l�@;d@�@
=@
=@�y@ȴ@�R@��@�+@v�@E�@{@{@@�T@��@@��@�h@`B@/@�@�/@�/@�@�/@�@I�@�@��@�F@t�@"�@o@�@�H@��@~�@^5@^5@M�@=q@J@��@��@�@�#@��@��@hs@G�@&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111-A�M�A�S�A�O�A�Q�A�K�A�33A��TAƸRAƮAƣ�Aƛ�AƍPA�t�A�=qA�-A��A�
=A��TAť�A�|�A�dZA�I�A�-A���A�v�A�jA�C�A�ĜA��9A��A�(�A���A�-A�9XA��A��HA��A��HA��A���A��/A�5?A���A�hsA�\)A�x�A��;A�A�A���A��!A��PA��hA�  A��\A�-A���A�?}A�bNA�bNA�ĜA��RA��/A��A��!A�A�A�ZA�33A���A�(�A� �A�%A��A��A��A�bA~�A}��A|��Az~�Aw�mAu`BAtQ�As�
Asl�Ar�\Ap��Ao
=Am�Ak��Aj{Ah��Ag�;Agt�Ag/Af1Ac�mAc
=AbVAb1'Aa�Aa%A`9XA_O�A^{A\��A\A[K�AZ�AZ1AYdZAX^5AW�AW�AU��ATĜATZATA�ATbAS�^ASXAS+AR�/ARQ�AQ�APz�AOC�AN1AL�HAK+AJffAIdZAH~�AG��AF�jAE��AE�AEADr�AC��AB�9AA�PA@�9A>�jA=��A<�/A;�PA;oA:��A:E�A9K�A8z�A7�mA7K�A6��A6jA6M�A5�^A4��A3�A2��A2��A1�wA1l�A133A0��A0�yA0�9A0M�A/�A/�
A/��A/XA/+A.��A.$�A,�\A+�mA+�
A+�A+�A*5?A(��A'��A'XA&ȴA&9XA%��A%��A$��A#�
A#G�A"�A"�HA"�jA"$�A!�A!XA bAp�AO�A"�AM�A�yA�+A9XAXAM�AG�A1'AXA%A��A�uAffA(�A�A�A�
AhsA�`Az�AJAƨA�hA�!AbA`BA��A�!AQ�A`BA~�AbNA
��A	�FA	dZA	"�A��AE�A��AK�A��A �A�A��A��A�`Az�AA
=A ff@�;d@��@��T@�%@��w@�-@��@���@��@�t�@�"�@�\@�/@�F@��@�Ĝ@�1'@�ƨ@���@�x�@�S�@�@�7L@�9@�Q�@��@��;@㕁@�o@��H@�ȴ@◍@��@�`B@�  @޸R@ۅ@�ff@�p�@���@֟�@��@���@�G�@�bN@�l�@��@̣�@˝�@�v�@Ɂ@�j@ǅ@�
=@ư!@���@ŉ7@�x�@���@�1@�~�@��/@�bN@�I�@�1@��F@��@�|�@�x�@��9@�9X@�;d@�~�@� �@�=q@�Ĝ@�bN@�9X@�  @���@���@���@�%@�9X@�b@��
@�t�@�dZ@��R@���@�`B@���@��
@��@�@�ȴ@�~�@�E�@�z�@�l�@���@��@���@���@�hs@�O�@�G�@�7L@��@���@���@�|�@��R@�=q@�p�@�9X@�|�@�C�@��@�=q@��-@�x�@�p�@�p�@�`B@�?}@�%@��@���@�I�@�1@��F@�|�@���@�v�@�{@���@�X@��@��@�t�@�
=@���@�n�@��#@��@�&�@��@�1@���@�"�@�~�@�M�@�J@��T@��7@���@��u@�bN@�I�@���@�ȴ@��@���@�X@��@�%@��@��/@��9@�(�@���@�t�@��@���@��R@���@��+@�n�@�ff@�M�@��#@��h@��@���@�K�@��H@�5?@��#@��^@��h@��@��j@���@��u@��@�r�@�b@K�@~�y@~V@}��@}/@|��@|��@|��@|��@{�@z�@y��@y�@x��@xr�@xA�@w�@wl�@w+@v�R@v��@vff@vV@vE�@v$�@u@up�@uO�@t��@t(�@sƨ@s�F@s��@s�@st�@sdZ@so@r�@q�7@n��@n�+@m�@m�-@m�-@m`B@l��@lj@k��@ko@j��@jM�@jJ@i�#@i��@iG�@h�`@hĜ@h�u@hr�@hA�@h1'@hr�@h1'@f��@e��@eO�@e?}@dZ@c�@cdZ@b��@co@a�^@a��@ax�@a�@`��@`�u@`1'@_�@^�y@^E�@^@]�h@]?}@]V@\�@\�@\�D@\1@[�@[C�@[@Z�H@Z��@Zn�@Y�@Y�^@Y��@Y��@Y��@Yhs@Y�@X�`@X��@X��@XQ�@Xb@W�w@WK�@WK�@V��@V�@V��@V5?@V{@V{@V{@U@U`B@U?}@U/@T�/@T��@T�j@T�@T�D@T(�@S�m@S��@R�H@RM�@Q�#@Qx�@Q&�@Q�@Q%@P��@P��@Pr�@Pb@O�P@Ol�@O+@O�@O�@N��@N��@NV@M@M`B@M�@L�/@L�j@L�D@LI�@L1@K��@KC�@J�@J�!@J�\@Jn�@Jn�@JM�@J=q@J=q@I�#@I�7@I%@H�9@HQ�@H  @G�w@G��@G+@G
=@F�y@Fȴ@F�R@F�+@F$�@E��@E�@EO�@EV@D��@D�j@D9X@C�
@C�F@C�F@C��@C��@C��@C��@Ct�@Co@B�H@B��@B��@Bn�@BM�@BJ@A�^@AG�@AG�@A7L@A%@@��@@Ĝ@@bN@?�@?��@?�@?��@?��@?��@?��@?�P@?+@>ȴ@>5?@=�T@=��@=@=@=�h@=`B@=`B@=O�@=?}@=�@<��@<��@<�@<�@<9X@;�@;dZ@;dZ@;S�@;C�@;"�@;@:��@:�!@:�\@:~�@:n�@:^5@:-@9�7@9%@8��@8�`@8��@8��@8�9@8��@8�u@8bN@8 �@7��@7|�@7K�@6��@6�y@6�@6��@6ff@5�T@5p�@5V@4�j@4(�@3C�@2�H@2��@2n�@2=q@1�#@1�7@17L@0��@0�@0b@/\)@/
=@.ȴ@-��@-V@,�/@,j@,�@+ƨ@+"�@+@*�!@*n�@*-@*J@)��@)�#@)��@)�7@)G�@(��@(r�@(r�@(A�@( �@( �@(  @'|�@&��@&v�@%�T@%@%�-@%p�@%�@$��@$��@$I�@$1@#�m@#ƨ@#�F@#�F@#S�@"�!@"M�@"=q@"=q@"=q@"=q@"=q@"=q@"-@"�@!��@!7L@ �`@ Ĝ@ Ĝ@ �9@ �u@ �@ r�@ Q�@ b@�w@�P@\)@�y@ȴ@�R@�R@�R@�R@��@��@��@��@��@��@�+@v�@ff@V@E�@�@@O�@��@�j@��@9X@��@��@ƨ@�@S�@"�@@��@~�@-@�#@��@X@Ĝ@��@�@Q�@�@l�@��@�@�R@�+@ff@ff@ff@E�@5?@@�@�T@��@p�@�@�@z�@I�@I�@(�@�F@��@dZ@S�@o@~�@n�@~�@�\@�\@J@x�@7L@&�@%@%@��@��@��@bN@ �@�@�;@��@�@�@�P@l�@
=@�R@ff@5?@�@��@�h@p�@`B@�@V@�@�/@�j@�@��@�D@z�@z�@j@�@��@dZ@C�@o@
�H@
=q@	��@	�#@	��@	x�@	7L@�9@Q�@�;@��@�w@�@�w@�w@�w@��@l�@;d@�@
=@
=@�y@ȴ@�R@��@�+@v�@E�@{@{@@�T@��@@��@�h@`B@/@�@�/@�/@�@�/@�@I�@�@��@�F@t�@"�@o@�@�H@��@~�@^5@^5@M�@=q@J@��@��@�@�#@��@��@hs@G�@&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111C�BS�BS�BT�BS�BS�BW
B\)B_;B_;B_;B_;B_;B^5B^5B^5B]/B\)BYBS�BP�BN�BM�BL�BH�BA�B0!B�#B�Bm�BffB_;BL�BC�BG�B@�B49B,B�BoB1BB��B�B�HB��B�^B�^B�FB�B��B�JB}�Bt�Bl�Be`BZBS�BD�B5?B-B�B�BPB%B
��B
�B
�BB
�B
��B
�}B
�'B
��B
��B
��B
��B
�VB
�+B
~�B
o�B
^5B
M�B
G�B
C�B
@�B
:^B
0!B
#�B
�B
\B
B	��B	��B	�B	�B	�B	�;B	�B	�B	��B	��B	��B	ɺB	ÖB	�jB	�9B	�B	��B	��B	��B	��B	��B	��B	�bB	�=B	�%B	�%B	�%B	�B	�B	� B	~�B	|�B	z�B	x�B	p�B	k�B	dZB	\)B	R�B	N�B	I�B	D�B	?}B	;dB	7LB	5?B	2-B	/B	,B	)�B	%�B	�B	�B	bB	JB	1B	1B	+B	B	B��B��B��B��B�B�B�B�B�fB�NB�BB�/B�#B�B�B�B�B�B��B��B��B��B��B��B��BŢBÖBB��B�wB�^B�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�hB�\B�JB�7B�+B�%B�B~�B{�By�Bw�Bw�Bv�Bu�Bt�Bs�Bp�Bm�Bl�Bk�BjBhsBhsBffBe`BcTBaHB_;B^5B]/B[#BXBW
BT�BQ�BO�BN�BM�BL�BK�BJ�BH�BG�BE�BC�B@�B?}B>wB=qB;dB9XB7LB6FB5?B5?B33B2-B/B-B,B+B+B)�B(�B'�B&�B&�B%�B$�B$�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B$�B%�B'�B'�B'�B&�B%�B&�B)�B,B-B-B/B.B1'B33B6FB7LB7LB7LB8RB8RB;dB>wBB�BD�BG�BK�BO�BN�BN�BN�BO�BT�BYBZB[#B\)B\)BbNBdZBffBiyBjBm�Bn�Bn�Bn�Bn�Bn�Bp�Bs�Bt�Bw�By�B� B�B�1B�7B�=B�VB�hB�oB�uB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�LB�RB�^B�qBBŢB��B��B��B��B��B�
B�B�)B�/B�5B�;B�sB�B��B��B��B��B��B��B��B	B	B	B		7B	
=B	
=B	DB	JB	PB	PB	VB	bB	hB	uB	�B	�B	�B	!�B	"�B	"�B	#�B	'�B	+B	,B	,B	-B	-B	0!B	33B	49B	6FB	9XB	;dB	=qB	>wB	?}B	?}B	A�B	E�B	F�B	G�B	G�B	H�B	I�B	K�B	M�B	P�B	T�B	VB	XB	XB	YB	[#B	]/B	]/B	]/B	`BB	cTB	dZB	e`B	e`B	e`B	e`B	ffB	gmB	gmB	hsB	k�B	jB	k�B	jB	jB	jB	jB	jB	k�B	l�B	m�B	o�B	p�B	q�B	r�B	s�B	t�B	u�B	v�B	w�B	|�B	� B	�B	�B	�B	�7B	�PB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�FB	�FB	�FB	�LB	�LB	�RB	�^B	�dB	�jB	�qB	�wB	��B	B	B	ÖB	ÖB	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
+B
,B
-B
-B
-B
/B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
E�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
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
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
l�B
l�B
l�B
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
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
t�B
t�B
t�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111C�BS�BS�BT�BT,BT{BW�B\xB_VB_VB_VB_pB_�B^�B^jB^jB]dB\�BY�BTaBQ4BO(BNpBNBK�BI7BC-B�B�}BuBjBbhBPHBFBJXBC-B6�B.}B!-B�B
XB�B��B��B�&B�{B�B�B�B�cB�;B�VBHBu�Bm�Bf�B[WBVBF�B7B/iB!�B$B�B�B 4B
��B
�B
�	B
�hB
��B
��B
��B
��B
��B
�B
��B
�B
��B
r�B
`�B
O(B
HfB
DgB
A�B
<jB
2|B
&B
kB
hB
�B	��B	�fB	��B	�MB	�B	�BB	��B	�mB	՛B	�,B	��B	�B	�9B	��B	�?B	�B	��B	��B	��B	��B	�_B	��B	�B	�^B	��B	�tB	�tB	��B	��B	�iB	}B	}�B	{�B	z�B	raB	m)B	fB	^B	TFB	P.B	KB	E�B	@�B	<PB	8B	5�B	3B	0UB	-]B	+�B	'mB	!�B	�B	�B	�B		B	�B	�B	YB	B��B��B��B�ZB�9B�B��B��B�B�B�HBݲBۦB�kB�eB�BؓBևB�gB�aB�[B�hBбB��B�~BƎB��B�BªB� B�B�zB��B��B��B��B��B�*B�B��B�'B��B�!B��B�CB��B�B�MB��B�B��B��B��B��B��B��B��B}<Bz�BxlBx8Bw2Bv+ButBt�Br-BnBmCBlWBk6BiBh�BgBf�BdZBb4B_�B^�B^B\xBYKBW�BV�BS[BPbBOvBN�BM�BL�BK^BI�BH�BF�BE�BA�B@ B?.B>]B<�B:^B8lB6�B5�B6+B4TB3�B1'B.cB,�B+kB+kB*�B*B)*B(
B'�B&fB%zB%�B#�B#TB!�B \B 'B 'B B 'BB;B�BB!B5B�B�B�B�B�B�B�B�BBeB=B�B�B�B�BxB�B~B�BOB;BBVB BB B!bB#�B%�B'B(XB($B(sB'�B'mB(�B+6B,�B-�B-�B0B/�B2|B4B6�B7�B7�B7�B9	B9>B<B>�BB�BD�BHBLBP}BO�BOBBOvBP�BU�BYeBZkB[�B\�B]dBc BeBf�Bi�Bj�Bm�Bn�Bn�Bn�Bn�BoBq'Bt9ButBxRBz�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�)B�/B�!B�4B�@B�fB��B��B�}B��B�|B��B��B��B��B��B��B�B�)B�B�@B�2BՁB�sBچB�xB�dB޸B�'B�B��B�B�B�	B��B�B�DB�PB	oB	�B	�B		RB	
rB	
rB	^B	dB	�B	�B	�B	�B	�B	aB	B	5B	;B	"B	#B	# B	$ZB	(>B	+B	,"B	,"B	-]B	-wB	0oB	3�B	4�B	6�B	9�B	;�B	=�B	>�B	?�B	@ B	BB	E�B	F�B	G�B	G�B	H�B	I�B	LB	NB	Q4B	U2B	VB	X+B	XEB	Y1B	[WB	]dB	]IB	]~B	`vB	c�B	d�B	ezB	ezB	e�B	ezB	f�B	g�B	h$B	i*B	k�B	j�B	k�B	j�B	j�B	j�B	j�B	j�B	k�B	l�B	m�B	o�B	p�B	q�B	r�B	s�B	t�B	u�B	v�B	xB	|�B	� B	�;B	��B	��B	��B	�jB	��B	��B	��B	��B	��B	�	B	��B	��B	�&B	��B	�8B	�>B	�DB	�WB	�iB	�GB	�hB	�nB	�zB	�`B	�`B	��B	��B	��B	�xB	�B	��B	��B	��B	��B	��B	��B	ÖB	ðB	��B	ðB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�.B	�B	�B	� B	� B	�B	�B	�2B	�9B	�SB	�?B	�EB	�eB	�kB	�xB	�dB	�OB	�;B	�VB	�VB	�VB	�\B	�B	�B	�B	�B	�zB	�fB	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	�B	�B	�"B	�B	�B	�.B	�.B	�B
 OB
;B
 B
 B
 B
AB
'B
[B
-B
3B
B
B
9B
B
?B
YB
_B
_B
1B
fB
KB
	RB
	RB
	RB
	RB
	7B
	RB

XB

XB
^B
xB
�B
�B
pB
VB
pB
VB
VB
�B
�B
�B
�B
}B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"B
"B
# B
$&B
%,B
&B
'B
'B
(
B
($B
)B
)B
*0B
+6B
+QB
,WB
-)B
-]B
-wB
/iB
0;B
0UB
1[B
1[B
2aB
3MB
3MB
3hB
4TB
4nB
5ZB
5ZB
5ZB
5ZB
5ZB
6zB
7�B
8RB
8lB
9rB
9XB
9�B
9�B
9�B
:�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
E�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
NB
NB
N�B
N�B
N�B
N�B
PB
O�B
QB
Q B
Q B
QB
R B
S&B
S&B
S@B
S@B
TB
UB
UB
UB
VB
VB
VB
VB
VB
V9B
VB
VB
V9B
V9B
W$B
WYB
X+B
X+B
YB
Y1B
YKB
Z7B
Z7B
Z7B
ZQB
ZQB
[=B
[#B
[#B
[=B
[WB
\xB
\]B
]/B
]IB
]/B
]/B
]IB
]IB
]~B
^OB
_pB
_VB
_;B
_VB
_;B
_VB
_VB
_�B
`vB
`\B
abB
abB
b�B
bNB
b�B
cTB
c�B
cTB
c�B
cTB
c�B
dtB
dZB
dZB
dtB
dZB
dtB
d�B
d�B
e�B
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
i�B
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
l�B
l�B
l�B
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
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
t�B
t�B
t�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�_<��<4;@<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808070035272018080700352720180807003527202211182135392022111821353920221118213539201808080016372018080800163720180808001637  JA  ARFMdecpA19c                                                                20180728003520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180727153547  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180727153548  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180727153549  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180727153549  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180727153549  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180727153549  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180727153549  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180727153550  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180727153550                      G�O�G�O�G�O�                JA  ARUP                                                                        20180727155453                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180727153500  CV  JULD            G�O�G�O�Fé                JM  ARCAJMQC2.0                                                                 20180806153527  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180806153527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180807151637  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123539  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                