CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-04-18T18:38:36Z creation;2018-04-18T18:38:45Z conversion to V3.1;2019-12-18T07:23:32Z update;2022-11-21T05:30:55Z update;     
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
resolution        =���   axis      Z        l  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  Mh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ̨   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180418183836  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_134                     2C  DdLYNAVIS_A                         0397                            ARGO 011514                     863 @�\"�|e�1   @�\#���@<�p:�~��dLYJ���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
@��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|�{D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�A�ĜA�A�A�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA���A��hA��#A��/A��A��A��+A�x�A�`BA�M�A�5?A�&�A� �A��A��A�ĜA���A���A���A��DA�I�A��A���A���A�v�A�E�A�-A���A�ZA��;A��A��A���A��A�ȴA���A��A�\)A��DA�-A�;dA��^A�E�A��A���A�VA�ZA�I�A�M�A���A�XA�C�A�;dA�oA�A�-A�`BA�hsA���A�/A��`A���A�33A�1'A�ffA�5?A��A�ƨA�-A�bA�Q�A���A���A�&�A�PAA|ȴAyx�Aw|�At�DAsAr(�Ap�An^5AmS�Al�uAkx�Ai��AgC�AeK�Ad  AcVAb�DAbM�Ab-AbAaoA^��A]/AZ=qAY33AX��AX5?AW�AV�ATM�AS7LAR��ARA�AQ��AQ��AQl�AQ+AP�!APA�AO�AL�RALffAK�AK��AKG�AJ��AI`BAI7LAIoAH��AH�9AH�DAHbAG�;AG��AE��AC��AA��AA�7A@��A?��A>ȴA=�A;�A;"�A:��A:VA9�FA8  A6�yA6�A6(�A6-A6$�A5�A5�PA533A4=qA3A3&�A2-A1�-A0��A/��A-�;A-?}A,��A,-A*�uA)"�A(�uA(A�A'��A'�hA'S�A'&�A&��A&��A&�uA&bNA&I�A&5?A%�TA$�yA#�mA"$�A ��A�hA33A��AjA�hA�HAJA�A{AS�A�A��A`BA+A�A�jAv�A�mAS�AE�A��AC�AjA�A��A33A�uA��AO�Az�A1'A-A1'AA��A
��A
$�A	/AXA{A��AA{A (�@���@�E�@���@�V@��@�l�@�+@�@���@�9X@�P@�C�@�\@��#@�O�@�(�@�G�@�C�@�ȴ@�E�@�@��@�hs@�/@���@�j@�Z@���@�ff@�j@�-@�|�@�p�@��@�@�V@؋D@���@�?}@Ӯ@��@�n�@�$�@�hs@���@���@��#@́@��@� �@���@�n�@�n�@�~�@�v�@�M�@�{@��T@î@§�@�@���@���@��@�7L@�bN@�S�@�@�bN@�;d@���@�v�@��h@�O�@�7L@��`@�dZ@�$�@�hs@�V@�  @�"�@�v�@�@�X@�A�@���@��@�  @�;d@�33@���@���@�n�@�J@���@��j@�I�@���@��@��P@�l�@�
=@���@��#@���@�X@�Ĝ@��D@�bN@��@��F@���@��P@�C�@���@��H@��@���@��@�`B@�&�@��@��@��@��/@��;@��@���@���@�j@�I�@�9X@��@���@�
=@��@��@�r�@���@�|�@�v�@�{@���@��u@��@�Q�@���@���@���@��@��P@�dZ@�+@�o@���@��+@�M�@�-@��@�@��@�@�p�@�&�@��@��@�%@�Ĝ@��D@�9X@���@��F@�\)@�C�@��y@�V@�5?@�5?@�5?@�-@�J@���@��^@���@�/@�Ĝ@��@��@�Q�@� �@��@���@��@��@���@���@�Ĝ@�Ĝ@�Ĝ@��j@���@�z�@�Z@�(�@�1@��F@�33@��R@�$�@�{@�J@�@���@��9@��u@��@�z�@�j@�Z@� �@�  @�@�;@�w@��@\)@+@~�@~ff@~$�@}�@}��@}�@}O�@}V@{��@z��@z-@z�@y��@y�@yhs@yG�@y7L@x��@x�u@x �@w�@v�R@v$�@u�@t�D@t1@s�m@s�m@s�
@s��@sC�@r��@r��@r~�@q�7@p��@p�u@pbN@o;d@n�y@n�y@n�y@n��@l�j@kƨ@kt�@kdZ@kS�@kC�@k33@j��@jJ@i�@i��@i�^@i��@i%@h�@h  @g�;@g�w@g��@gl�@g;d@g+@g
=@f��@fȴ@f��@fv�@fff@f{@e�@e?}@d�@dz�@d(�@c��@c�m@cƨ@c��@ct�@ct�@co@b��@b^5@a��@a��@`�u@_�;@_l�@_;d@_+@^�y@^ȴ@^�+@^V@]�@]��@]?}@]/@]/@]�@\��@\��@\j@\(�@\�@[��@Z��@Z=q@YX@X��@X�@X �@X  @W�@W�w@W��@W\)@W�@W
=@Vȴ@Vv�@VV@V5?@V{@V@U�@U�T@U�h@UO�@T�@T�j@Tz�@T�@Sƨ@S��@St�@So@R�!@R=q@Q��@Qx�@P�`@P�@PQ�@PA�@P1'@P �@P  @O�@O\)@OK�@N�@N��@N�+@Nff@N$�@N@M�T@MO�@L�/@L��@L�D@LI�@K��@K�@KC�@J�H@J-@I�^@Ix�@H��@H�9@Hr�@H1'@G�@G��@G�@G��@G;d@F��@FV@F5?@E�@E�-@E�-@E�@E`B@E?}@D��@D(�@D�@D1@C�F@B��@B�\@BJ@Ax�@AG�@A�@@��@@��@@�`@@�`@@��@@A�@?�@?�;@?��@?;d@>��@>{@=�@<Z@<�@;�F@;�@;S�@:��@:�@9�#@9G�@9�@8Ĝ@8Q�@8A�@8A�@8A�@8A�@8b@8b@8b@8b@7�@7��@7�@7�P@7�P@7|�@7�P@7l�@7;d@7
=@6ȴ@6v�@6V@6$�@5��@5�@5p�@5`B@5O�@5O�@5O�@5?}@5/@5V@4�/@4z�@3�m@3C�@3o@3@2~�@2-@2J@1��@1X@1�@1%@0�`@0�@0bN@0A�@01'@0  @/��@/|�@/�@.�@.v�@.E�@.5?@.$�@.{@-�@-�T@-��@-�h@-O�@,��@,�@,z�@,(�@+�F@+o@*��@*�@*��@*�!@*~�@*~�@*^5@*�@)��@)%@(��@(r�@(bN@(bN@(Q�@(1'@( �@'�@'�w@'�w@'��@'l�@'+@&�+@&E�@&E�@&{@%��@%�h@%`B@%/@$��@$I�@$(�@$�@#�m@#�
@#�
@#��@#��@#dZ@#o@"�@"��@"n�@"-@"�@"J@!��@!��@!hs@!�@ �9@ b@��@�@|�@\)@;d@��@ff@E�@5?@@�T@��@@�-@�h@�@p�@p�@`B@O�@?}@?}@/@/@V@�@��@�@��@�D@j@Z@�@��@S�@�H@^5@�@��@��@G�@%@�`@Ĝ@�u@bN@ �@�@�w@l�@��@{@�-@�@9X@(�@1@�m@�
@ƨ@��@t�@S�@"�@@�H@��@n�@M�@-@�@��@��@��@�7@X@7L@�@�@%@%@��@�`@��@�9@��@�@bN@A�@1'@ �@  @  @�@��@�w@��@�R@�T@�-@��@�h@�@p�@O�@?}@?}@/@/@�@��@�@�/@�/@��@�j@�D@Z@�@1@1@��@�m@�
@��@
�H@
n�@
=q@
J@	�@	��@	��@	x�@	&�@	%@	%@�`@�9@�u@1'@ �@ �@b@b@  @�@\)@ȴ@��@v�@ff@V@5?@$�@{@@�@�@��@@�-@��@�h@�h@�@�@��@�@�D@z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�A�ĜA�A�A�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA���A��hA��#A��/A��A��A��+A�x�A�`BA�M�A�5?A�&�A� �A��A��A�ĜA���A���A���A��DA�I�A��A���A���A�v�A�E�A�-A���A�ZA��;A��A��A���A��A�ȴA���A��A�\)A��DA�-A�;dA��^A�E�A��A���A�VA�ZA�I�A�M�A���A�XA�C�A�;dA�oA�A�-A�`BA�hsA���A�/A��`A���A�33A�1'A�ffA�5?A��A�ƨA�-A�bA�Q�A���A���A�&�A�PAA|ȴAyx�Aw|�At�DAsAr(�Ap�An^5AmS�Al�uAkx�Ai��AgC�AeK�Ad  AcVAb�DAbM�Ab-AbAaoA^��A]/AZ=qAY33AX��AX5?AW�AV�ATM�AS7LAR��ARA�AQ��AQ��AQl�AQ+AP�!APA�AO�AL�RALffAK�AK��AKG�AJ��AI`BAI7LAIoAH��AH�9AH�DAHbAG�;AG��AE��AC��AA��AA�7A@��A?��A>ȴA=�A;�A;"�A:��A:VA9�FA8  A6�yA6�A6(�A6-A6$�A5�A5�PA533A4=qA3A3&�A2-A1�-A0��A/��A-�;A-?}A,��A,-A*�uA)"�A(�uA(A�A'��A'�hA'S�A'&�A&��A&��A&�uA&bNA&I�A&5?A%�TA$�yA#�mA"$�A ��A�hA33A��AjA�hA�HAJA�A{AS�A�A��A`BA+A�A�jAv�A�mAS�AE�A��AC�AjA�A��A33A�uA��AO�Az�A1'A-A1'AA��A
��A
$�A	/AXA{A��AA{A (�@���@�E�@���@�V@��@�l�@�+@�@���@�9X@�P@�C�@�\@��#@�O�@�(�@�G�@�C�@�ȴ@�E�@�@��@�hs@�/@���@�j@�Z@���@�ff@�j@�-@�|�@�p�@��@�@�V@؋D@���@�?}@Ӯ@��@�n�@�$�@�hs@���@���@��#@́@��@� �@���@�n�@�n�@�~�@�v�@�M�@�{@��T@î@§�@�@���@���@��@�7L@�bN@�S�@�@�bN@�;d@���@�v�@��h@�O�@�7L@��`@�dZ@�$�@�hs@�V@�  @�"�@�v�@�@�X@�A�@���@��@�  @�;d@�33@���@���@�n�@�J@���@��j@�I�@���@��@��P@�l�@�
=@���@��#@���@�X@�Ĝ@��D@�bN@��@��F@���@��P@�C�@���@��H@��@���@��@�`B@�&�@��@��@��@��/@��;@��@���@���@�j@�I�@�9X@��@���@�
=@��@��@�r�@���@�|�@�v�@�{@���@��u@��@�Q�@���@���@���@��@��P@�dZ@�+@�o@���@��+@�M�@�-@��@�@��@�@�p�@�&�@��@��@�%@�Ĝ@��D@�9X@���@��F@�\)@�C�@��y@�V@�5?@�5?@�5?@�-@�J@���@��^@���@�/@�Ĝ@��@��@�Q�@� �@��@���@��@��@���@���@�Ĝ@�Ĝ@�Ĝ@��j@���@�z�@�Z@�(�@�1@��F@�33@��R@�$�@�{@�J@�@���@��9@��u@��@�z�@�j@�Z@� �@�  @�@�;@�w@��@\)@+@~�@~ff@~$�@}�@}��@}�@}O�@}V@{��@z��@z-@z�@y��@y�@yhs@yG�@y7L@x��@x�u@x �@w�@v�R@v$�@u�@t�D@t1@s�m@s�m@s�
@s��@sC�@r��@r��@r~�@q�7@p��@p�u@pbN@o;d@n�y@n�y@n�y@n��@l�j@kƨ@kt�@kdZ@kS�@kC�@k33@j��@jJ@i�@i��@i�^@i��@i%@h�@h  @g�;@g�w@g��@gl�@g;d@g+@g
=@f��@fȴ@f��@fv�@fff@f{@e�@e?}@d�@dz�@d(�@c��@c�m@cƨ@c��@ct�@ct�@co@b��@b^5@a��@a��@`�u@_�;@_l�@_;d@_+@^�y@^ȴ@^�+@^V@]�@]��@]?}@]/@]/@]�@\��@\��@\j@\(�@\�@[��@Z��@Z=q@YX@X��@X�@X �@X  @W�@W�w@W��@W\)@W�@W
=@Vȴ@Vv�@VV@V5?@V{@V@U�@U�T@U�h@UO�@T�@T�j@Tz�@T�@Sƨ@S��@St�@So@R�!@R=q@Q��@Qx�@P�`@P�@PQ�@PA�@P1'@P �@P  @O�@O\)@OK�@N�@N��@N�+@Nff@N$�@N@M�T@MO�@L�/@L��@L�D@LI�@K��@K�@KC�@J�H@J-@I�^@Ix�@H��@H�9@Hr�@H1'@G�@G��@G�@G��@G;d@F��@FV@F5?@E�@E�-@E�-@E�@E`B@E?}@D��@D(�@D�@D1@C�F@B��@B�\@BJ@Ax�@AG�@A�@@��@@��@@�`@@�`@@��@@A�@?�@?�;@?��@?;d@>��@>{@=�@<Z@<�@;�F@;�@;S�@:��@:�@9�#@9G�@9�@8Ĝ@8Q�@8A�@8A�@8A�@8A�@8b@8b@8b@8b@7�@7��@7�@7�P@7�P@7|�@7�P@7l�@7;d@7
=@6ȴ@6v�@6V@6$�@5��@5�@5p�@5`B@5O�@5O�@5O�@5?}@5/@5V@4�/@4z�@3�m@3C�@3o@3@2~�@2-@2J@1��@1X@1�@1%@0�`@0�@0bN@0A�@01'@0  @/��@/|�@/�@.�@.v�@.E�@.5?@.$�@.{@-�@-�T@-��@-�h@-O�@,��@,�@,z�@,(�@+�F@+o@*��@*�@*��@*�!@*~�@*~�@*^5@*�@)��@)%@(��@(r�@(bN@(bN@(Q�@(1'@( �@'�@'�w@'�w@'��@'l�@'+@&�+@&E�@&E�@&{@%��@%�h@%`B@%/@$��@$I�@$(�@$�@#�m@#�
@#�
@#��@#��@#dZ@#o@"�@"��@"n�@"-@"�@"J@!��@!��@!hs@!�@ �9@ b@��@�@|�@\)@;d@��@ff@E�@5?@@�T@��@@�-@�h@�@p�@p�@`B@O�@?}@?}@/@/@V@�@��@�@��@�D@j@Z@�@��@S�@�H@^5@�@��@��@G�@%@�`@Ĝ@�u@bN@ �@�@�w@l�@��@{@�-@�@9X@(�@1@�m@�
@ƨ@��@t�@S�@"�@@�H@��@n�@M�@-@�@��@��@��@�7@X@7L@�@�@%@%@��@�`@��@�9@��@�@bN@A�@1'@ �@  @  @�@��@�w@��@�R@�T@�-@��@�h@�@p�@O�@?}@?}@/@/@�@��@�@�/@�/@��@�j@�D@Z@�@1@1@��@�m@�
@��@
�H@
n�@
=q@
J@	�@	��@	��@	x�@	&�@	%@	%@�`@�9@�u@1'@ �@ �@b@b@  @�@\)@ȴ@��@v�@ff@V@5?@$�@{@@�@�@��@@�-@��@�h@�h@�@�@��@�@�D@z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�}B�}B�}B�}B�}B�}B�}B�}B�}B�wB�^B�!B��B��B�uB�\B�\B�VB�PB�JB�DB�DB�=B�=B�1B�+B�B�B�B�B� B}�By�Bw�Bu�Bs�Br�Bo�BhsB`BBJ�B2-B#�B�BbBJBBB��B�BB��BÖB�^B�3B�B��B�oB}�Bn�BffB]/B[#BZBW
BP�BG�B;dB,B�B�BoBPBB
��B
�mB
�ZB
�NB
�)B
��B
B
�B
��B
��B
��B
�{B
�VB
~�B
jB
[#B
I�B
?}B
9XB
,B
 �B
�B
{B
PB
B	��B	�yB	�NB	�/B	�B	�B	�
B	��B	��B	�}B	�?B	��B	��B	��B	��B	�{B	�JB	�B	|�B	y�B	w�B	u�B	t�B	s�B	q�B	o�B	l�B	gmB	[#B	XB	VB	S�B	Q�B	N�B	J�B	I�B	H�B	H�B	F�B	E�B	C�B	@�B	=qB	5?B	+B	"�B	�B	�B	�B	oB	JB	%B	B	  B��B��B��B�B�B�B�B�B�B�B�sB�ZB�NB�;B�#B�B��B��BǮBŢBB�}B�^B�?B�3B�-B�!B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�oB�bB�\B�PB�DB�1B�B�B� B|�Bz�By�Bw�Bw�Bv�Bu�Bt�Br�Bp�Bm�BiyBe`BcTBbNBaHB`BB^5B]/B[#BYBYBYBXBW
BVBS�BQ�BN�BJ�BG�BD�BA�B=qB;dB:^B8RB6FB5?B5?B49B33B2-B1'B1'B0!B0!B/B.B-B+B)�B)�B(�B(�B(�B(�B(�B(�B'�B'�B&�B&�B%�B$�B#�B#�B"�B"�B#�B$�B#�B$�B$�B%�B%�B%�B%�B%�B$�B$�B%�B$�B$�B$�B%�B%�B%�B%�B$�B$�B!�B$�B%�B%�B%�B%�B%�B%�B&�B'�B+B-B.B1'B1'B2-B33B33B33B33B5?B6FB6FB6FB7LB9XB;dB<jB=qB?}BD�BE�BK�BM�BM�BN�BO�BO�BP�BW
BW
BYBZB[#B[#B[#B[#B[#B]/B_;B`BBbNBcTBdZBffBl�Bn�Bo�Bp�Br�Bs�Bs�Bs�Bt�Bw�B|�B�B�%B�%B�7B�PB�hB��B��B��B��B��B��B��B��B�B�B�3B�?B�LB�XB�^B��B��B��BBǮBɺB��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�#B�5B�5B�5B�5B�BB�HB�TB�`B�mB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	+B	
=B	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	)�B	0!B	0!B	0!B	2-B	8RB	:^B	;dB	<jB	<jB	<jB	=qB	>wB	@�B	@�B	@�B	A�B	A�B	B�B	C�B	D�B	F�B	G�B	H�B	H�B	I�B	K�B	L�B	S�B	W
B	ZB	ZB	[#B	[#B	]/B	]/B	^5B	_;B	`BB	bNB	dZB	hsB	jB	l�B	p�B	r�B	s�B	s�B	s�B	t�B	u�B	w�B	x�B	x�B	|�B	� B	�B	�B	�%B	�+B	�+B	�+B	�+B	�\B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�^B	�dB	�wB	��B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
PB
PB
PB
PB
bB
bB
bB
bB
oB
oB
hB
hB
hB
oB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
%�B
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
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
7LB
8RB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
W
B
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
ZB
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
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
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
e`B
ffB
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
jB
jB
jB
jB
jB
jB
jB
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�}B�}B��B�}B�}B�}B�}B��B��B��B�<B��B��B�#B�FB��B��B��B��B�~B�^B�^B�rB��B��B�_B�9B�3B�gB��B��B~�BzDBx8Bv+BtBs�BqABjBdZBOB5�B&�B�BNB"BgB_B��B�NB�BĜB�0B�9B��B��B��B�Bo�BgRB]~B[WBZ�BXBR�BI�B=�B-�B�BEB[B�B_B
�`B
�
B
��B
�B
�B
��B
��B
�cB
��B
��B
��B
�B
�NB
��B
mCB
^5B
K�B
AB
;�B
.B
"B
�B
B
�B
%B	��B	�B	�TB	��B	ڠB	�eB	��B	��B	ϫB	��B	�RB	�B	��B	�xB	��B	�SB	�pB	�MB	}�B	zxB	xRB	v+B	uB	t9B	raB	p�B	nB	jB	[�B	X�B	V�B	T�B	R�B	PB	KB	I�B	IB	IB	GB	FYB	DB	A�B	?�B	7�B	,�B	#�B	 �B	B	1B	,B	B	EB	�B	 �B�.B��B��B�'B�B��B��B��B�)B�QB�B�,B�TB��B�)B�QB��B��B��BƎBÖB��B�B�+B��B��B��B��B�}B�cB�]B��B�qB�QB�eB��B�XB�zB��B�xB��B�B�B�HB��B�dB��B��B�GB�;B~B{�BzxBx8Bx8Bw2Bv`Bu�Bs�BrGBo�BkQBf�BdBb�BbBaHB_VB^B\BYBYKBYKBX�BW�BW$BUMBS�BQ4BL�BI�BF%BD3B?�B<6B;�B:^B7fB6B5�B4�B4B2�B1�B1�B0�B0�B/�B.�B./B,�B+QB*eB)_B)DB)DB)DB)_B)DB(XB(sB'�B(>B'�B&�B%�B%FB$B$&B$�B%�B%FB&B%�B&�B&LB&LB&�B&�B&B%�B&fB%zB%�B%�B&B%�B%�B&B%FB%�B$ZB&fB&�B&�B&�B'8B&�B&�B'�B(�B,"B./B.�B1vB1�B2�B3�B3�B3�B4nB6B6�B6�B7B8B9�B;�B="B>]B@�BEmBF�BLdBNBN<BO(BP.BP}BQ�BW?BWsBYeBZkB[qB[WB[�B[�B[�B]�B_�B`�Bb�Bc�Bd�Bf�Bl�Bn�Bo�BqBr�Bs�BtBtTBu%BxB}<B�B�?B�tB�	B�"B�oB�)B�'B��B��B�B�NB�zB��B�wB��B��B��B�B��B�0B��B��B��B��B��BɺB��B�B�"B�B��B�B�4B�,B�B�$B�?B�EB�_B�B�WB�OB�OB�OBޞB�vB�B�B�B�B�B��B��B��B��B��B��B��B�B��B��B�2B�$B�B�"B�(B�cB	 �B	�B	
�B	 B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	 'B	#TB	'RB	*eB	0;B	0;B	0oB	2�B	8�B	:�B	;�B	<�B	<�B	<�B	=�B	>�B	@�B	@�B	@�B	A�B	A�B	B�B	C�B	D�B	F�B	G�B	H�B	IB	J	B	LB	MjB	TaB	W?B	Z7B	Z7B	[=B	[WB	]IB	]IB	^jB	_pB	`vB	b�B	d�B	h�B	j�B	l�B	p�B	r�B	s�B	s�B	s�B	uB	u�B	w�B	y	B	y>B	}<B	�4B	�;B	��B	�YB	�+B	�EB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	��B	�B	�B	�$B	�B	�B	�*B	�0B	�"B	�]B	�OB	�UB	�aB	�MB	�MB	�MB	�TB	�tB	�tB	�zB	��B	��B	��B	��B	��B	��B	ðB	ðB	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�BB	�&B	�MB	�?B	�1B	�7B	�=B	�]B	�CB	�dB	�dB	�OB	�jB	�OB	�VB	�vB	�vB	�bB	�hB	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	�	B	�B	�B	�B	�"B	�"B	�BB	�B
 OB
 iB
AB
-B
MB
SB
?B
YB
EB
_B
fB
fB
fB
	lB

XB
^B
xB
dB
dB
dB
�B
jB
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
!B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$B
$B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
&B
%�B
'B
'B
'8B
(
B
'�B
(
B
'�B
(
B
'�B
(
B
(
B
($B
($B
)DB
)DB
*0B
*B
+6B
+6B
,=B
,"B
,"B
,=B
-CB
./B
.IB
.IB
/OB
/OB
0UB
0;B
0;B
0;B
1AB
1AB
2aB
3hB
3MB
33B
3MB
3MB
3MB
3hB
3MB
4nB
4nB
5tB
5ZB
5�B
5tB
6zB
6`B
6`B
7�B
8lB
7fB
8lB
7�B
7fB
7�B
8�B
9rB
:xB
:^B
:xB
:^B
:�B
;B
;B
;B
<jB
<�B
<�B
<�B
=�B
>�B
>wB
>�B
>�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
KB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
L�B
M�B
M�B
M�B
M�B
NB
NB
M�B
NB
OB
OB
PB
Q B
Q B
Q B
RB
R B
SB
SB
S&B
S&B
T,B
TB
TB
U2B
UMB
V9B
V9B
VSB
W?B
XB
X+B
Y1B
Y1B
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
ZQB
ZQB
[=B
[=B
[WB
[=B
[=B
\]B
\]B
\]B
\CB
]IB
]IB
]IB
]/B
]/B
]IB
]/B
]/B
]dB
]dB
^OB
^jB
^jB
^5B
^OB
^OB
_;B
_;B
_pB
_VB
_pB
_�B
`�B
a|B
bhB
bNB
bhB
bNB
b�B
bNB
cnB
cnB
cTB
cTB
c�B
cTB
cnB
cTB
cTB
c�B
cnB
dtB
dtB
dtB
dZB
dZB
dZB
d�B
dtB
e�B
f�B
g�B
g�B
g�B
g�B
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
j�B
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804290036362018042900363620180429003636202211182134262022111821342620221118213426201806041923482018060419234820180604192348  JA  ARFMdecpA19c                                                                20180419033620  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180418183836  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180418183839  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180418183840  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180418183843  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180418183843  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180418183843  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180418183843  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180418183845  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180418183845                      G�O�G�O�G�O�                JA  ARUP                                                                        20180418191754                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180419153159  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180428153636  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180428153636  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604102348  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123426  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                