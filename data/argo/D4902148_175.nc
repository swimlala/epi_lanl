CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-06-02T18:38:43Z creation;2019-06-02T18:38:47Z conversion to V3.1;2019-12-18T07:14:43Z update;2022-11-21T05:28:52Z update;     
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
_FillValue                 �  ]0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20190602183843  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_175                     2C  DdAfNAVIS_A                         0397                            ARGO 011514                     863 @�¢��?�1   @�£Q�n @;����m]�dAf�A�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��
@��
@�p�A Q�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`{Bg�Bo�Bw�B�B��
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
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{Dz�D��Dt{D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z=D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�@�D�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��;A�ffA�ffA�bNA�dZA�XA�&�A�\)A�O�A���A���A���A�dZA�S�A���A���A�+A�ȴA��A�t�A� �A�hsA��A��A�
=A��hA��
A��DA�5?A�ZA�ƨA�33A���A��;A�x�A�%A���A���A�?}A���A��-A���A�dZA��A��A��
A���A�K�A��A�v�A�Q�A�7LA��A�A�A��;A��PA��RA��wA���A�I�A���A���A���A��TA�{A��A���A���A�/A��A��A��A�t�A��9A�5?A}/Az�Awp�Au|�AuoAs�
AqhsApbAox�Anz�Al�HAk�AkK�Aj��Ai�TAi7LAh1'Agl�Aex�Ab�Aa�Aal�AaK�A`�A_��A_
=A]��A\ȴA\1A[
=AZ$�AX��AW�wAU��AT�uASS�ARȴARQ�AQ�AQ33APĜAPv�AP{AO��AN=qALJAJ{AIp�AH��AGAF�yAF�`AFr�AE`BAC�-AC%ABȴAB�AA��AAx�AA&�A@��A?��A?t�A?A>��A>bNA=�-A<�A;�A9A8E�A6��A5t�A4(�A1x�A01A/ƨA/�PA/O�A.��A-/A+��A+33A*v�A)�;A)C�A(5?A'oA&bA%G�A$��A$��A$(�A#�
A#�^A#l�A#+A"��A"�A"Q�A!�A!&�AS�A�jA�uAv�AjA1'A1A�FAt�A�A{A�\Ar�A-Al�Ar�A��AG�A&�A%A�A�9AI�A��A�/A�mA�A�A��A �A�`A9XAhsA��AA�AS�AoA
ĜA	�;A	��A	dZA�!A`BA��A  A��A�A$�A��AXA��Az�AQ�A$�A  A�
AƨA�A �@�\)@�\)@�
=@��@�S�@�^@�&�@�Ĝ@��@�9X@�ƨ@�l�@�@�r�@땁@�n�@��@�Z@��H@���@�$�@�7@���@��;@߶F@ߕ�@��@�7L@��@�/@��@��@��H@ְ!@�%@�Q�@�K�@�ff@�E�@�{@ЋD@��
@Χ�@͡�@�|�@���@��@�x�@���@��m@Ɵ�@��@�?}@ēu@Õ�@�J@���@��@��9@�b@���@���@�-@�x�@�Ĝ@��
@�S�@�~�@�@�&�@�Z@��
@�dZ@�33@��H@�7L@�Z@��@��!@���@�p�@�7L@��`@��@��H@�5?@���@��j@��F@�C�@�@��R@���@�ff@�$�@���@��@�V@�Ĝ@���@�t�@��H@�M�@���@��
@�+@���@��@��7@���@��u@�Z@���@��@�l�@�S�@�C�@�;d@�@���@�=q@���@��@���@�z�@�I�@��;@�|�@��y@��+@�J@��@��@���@���@���@���@��h@��@��9@��u@�Q�@�1'@��@��m@���@�K�@�+@�
=@��H@�$�@�@��h@���@���@�Q�@� �@�t�@��@���@��@���@�?}@�Z@��@�\)@�;d@���@���@�M�@�$�@���@�`B@�&�@��@��@�%@��/@��j@���@��D@�j@�  @�t�@�33@��y@�M�@��@���@���@���@���@���@���@���@���@���@���@���@���@��j@���@���@���@���@���@���@���@��@�bN@�Z@�I�@��@K�@
=@~�@~v�@~{@}��@}�@}p�@}/@|�j@|1@{t�@{@z^5@z�@y�@y��@y��@y�^@y��@y��@y��@y��@y��@yG�@y%@xĜ@x�@x1'@w�@w|�@v��@vE�@v5?@u�@t��@t�@tz�@t9X@s��@s��@st�@s"�@r�!@rn�@rM�@r�@q�@qhs@p��@p��@pĜ@p�9@p��@o��@n�y@m@l�@l��@l(�@l1@k��@kt�@kC�@ko@j~�@jJ@ix�@h��@h �@g�w@gl�@fȴ@fff@e��@e�h@e@e�h@e��@e@e@e?}@d��@c�
@c@b��@b�!@b�\@bJ@a&�@`Q�@_��@_\)@^v�@^{@]�-@]�h@]/@\Z@\�@[��@["�@Z�@Zn�@ZJ@Y�^@YX@Y%@XQ�@Xb@X  @W��@W�P@W+@Vv�@Up�@U�@T��@T�/@Tj@SdZ@R�H@R�!@R�\@R=q@Qx�@QG�@QG�@Q7L@Q�@PĜ@PQ�@P1'@Pb@O��@O��@O�P@Ol�@O;d@N�y@N�@N��@M�@MO�@L��@L�@Lz�@LI�@L�@K��@K��@K�
@K��@KdZ@Ko@J�H@J�H@J�H@J��@J��@J��@J��@J�!@J�\@JM�@I��@I��@IG�@I%@HĜ@HbN@H �@G�;@G;d@F5?@D�D@C�m@C�@CdZ@C33@C@B�H@B~�@BM�@B=q@B=q@B�@A�7@A&�@@��@@��@@�@?�;@?�w@?K�@>�R@>V@>$�@=��@=�-@=O�@<��@<�j@<�@<j@<I�@;��@;ƨ@;��@;S�@:�@:��@:n�@:^5@:^5@:M�@:M�@:�@9��@9��@9�7@9x�@9X@9G�@97L@9&�@8��@8�9@8 �@7�@7��@7�P@7�P@7|�@7l�@7\)@6��@5@4��@49X@4(�@4�@4�@4�@4�@4�@4�@3�m@3ƨ@3dZ@3"�@2�H@2�!@2^5@2�@2J@1�#@1��@1�7@1&�@0�9@0r�@0A�@/�w@/|�@/\)@/+@/�@/
=@.��@.��@.�y@.ȴ@.��@.��@.��@.��@.V@.5?@-�T@,�/@,I�@+�@*��@*�@)�7@)x�@)X@)7L@)&�@)�@)%@(�`@(�9@(�@(A�@(  @'��@'�P@'K�@&�y@&�R@&�+@&V@&{@%p�@$��@$�/@$�/@$�@$�D@$z�@$Z@$�@$�@#��@#�
@#ƨ@#�F@#dZ@#@"�\@"n�@"=q@!��@!�@!�#@!�^@!x�@!&�@ ��@ r�@l�@�R@��@v�@5?@�T@@�@`B@�@�@V@�@��@I�@�m@�
@�F@�@C�@"�@o@�H@��@~�@-@-@�@��@��@x�@X@7L@�9@ �@  @�;@l�@K�@;d@+@+@
=@�R@ff@E�@5?@$�@{@{@{@{@{@{@@@�@��@�h@`B@/@�/@��@��@9X@(�@��@ƨ@��@t�@33@@�H@��@~�@n�@^5@M�@=q@=q@-@��@�7@�@��@�9@r�@A�@1'@b@  @  @�@�@�@�@��@�w@�@��@|�@;d@�@
=@�y@�@��@�+@v�@ff@ff@V@V@$�@�T@@@@�-@��@p�@O�@/@�/@�@Z@(�@�@�
@��@�@"�@o@@
�H@
��@
��@
��@
��@
�!@
��@
��@
��@
�\@
~�@
M�@
�@	�#@	��@	��@	x�@	�@��@�9@��@�u@r�@bN@Q�@ �@ �@ �@b@�;@�@|�@\)@
=@��@v�@E�@5?@$�@{@{@@�@@�@p�@�@�/@�/@z�@1@�F@�@dZ@C�@"�@��@��@~�@n�@^5@-@�@�#@��@��@�^@�^@��@��@�7@G�@ ��@ Ĝ@ ��@ Q�@ b?���?�\)?��?���?��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��;A�ffA�ffA�bNA�dZA�XA�&�A�\)A�O�A���A���A���A�dZA�S�A���A���A�+A�ȴA��A�t�A� �A�hsA��A��A�
=A��hA��
A��DA�5?A�ZA�ƨA�33A���A��;A�x�A�%A���A���A�?}A���A��-A���A�dZA��A��A��
A���A�K�A��A�v�A�Q�A�7LA��A�A�A��;A��PA��RA��wA���A�I�A���A���A���A��TA�{A��A���A���A�/A��A��A��A�t�A��9A�5?A}/Az�Awp�Au|�AuoAs�
AqhsApbAox�Anz�Al�HAk�AkK�Aj��Ai�TAi7LAh1'Agl�Aex�Ab�Aa�Aal�AaK�A`�A_��A_
=A]��A\ȴA\1A[
=AZ$�AX��AW�wAU��AT�uASS�ARȴARQ�AQ�AQ33APĜAPv�AP{AO��AN=qALJAJ{AIp�AH��AGAF�yAF�`AFr�AE`BAC�-AC%ABȴAB�AA��AAx�AA&�A@��A?��A?t�A?A>��A>bNA=�-A<�A;�A9A8E�A6��A5t�A4(�A1x�A01A/ƨA/�PA/O�A.��A-/A+��A+33A*v�A)�;A)C�A(5?A'oA&bA%G�A$��A$��A$(�A#�
A#�^A#l�A#+A"��A"�A"Q�A!�A!&�AS�A�jA�uAv�AjA1'A1A�FAt�A�A{A�\Ar�A-Al�Ar�A��AG�A&�A%A�A�9AI�A��A�/A�mA�A�A��A �A�`A9XAhsA��AA�AS�AoA
ĜA	�;A	��A	dZA�!A`BA��A  A��A�A$�A��AXA��Az�AQ�A$�A  A�
AƨA�A �@�\)@�\)@�
=@��@�S�@�^@�&�@�Ĝ@��@�9X@�ƨ@�l�@�@�r�@땁@�n�@��@�Z@��H@���@�$�@�7@���@��;@߶F@ߕ�@��@�7L@��@�/@��@��@��H@ְ!@�%@�Q�@�K�@�ff@�E�@�{@ЋD@��
@Χ�@͡�@�|�@���@��@�x�@���@��m@Ɵ�@��@�?}@ēu@Õ�@�J@���@��@��9@�b@���@���@�-@�x�@�Ĝ@��
@�S�@�~�@�@�&�@�Z@��
@�dZ@�33@��H@�7L@�Z@��@��!@���@�p�@�7L@��`@��@��H@�5?@���@��j@��F@�C�@�@��R@���@�ff@�$�@���@��@�V@�Ĝ@���@�t�@��H@�M�@���@��
@�+@���@��@��7@���@��u@�Z@���@��@�l�@�S�@�C�@�;d@�@���@�=q@���@��@���@�z�@�I�@��;@�|�@��y@��+@�J@��@��@���@���@���@���@��h@��@��9@��u@�Q�@�1'@��@��m@���@�K�@�+@�
=@��H@�$�@�@��h@���@���@�Q�@� �@�t�@��@���@��@���@�?}@�Z@��@�\)@�;d@���@���@�M�@�$�@���@�`B@�&�@��@��@�%@��/@��j@���@��D@�j@�  @�t�@�33@��y@�M�@��@���@���@���@���@���@���@���@���@���@���@���@���@��j@���@���@���@���@���@���@���@��@�bN@�Z@�I�@��@K�@
=@~�@~v�@~{@}��@}�@}p�@}/@|�j@|1@{t�@{@z^5@z�@y�@y��@y��@y�^@y��@y��@y��@y��@y��@yG�@y%@xĜ@x�@x1'@w�@w|�@v��@vE�@v5?@u�@t��@t�@tz�@t9X@s��@s��@st�@s"�@r�!@rn�@rM�@r�@q�@qhs@p��@p��@pĜ@p�9@p��@o��@n�y@m@l�@l��@l(�@l1@k��@kt�@kC�@ko@j~�@jJ@ix�@h��@h �@g�w@gl�@fȴ@fff@e��@e�h@e@e�h@e��@e@e@e?}@d��@c�
@c@b��@b�!@b�\@bJ@a&�@`Q�@_��@_\)@^v�@^{@]�-@]�h@]/@\Z@\�@[��@["�@Z�@Zn�@ZJ@Y�^@YX@Y%@XQ�@Xb@X  @W��@W�P@W+@Vv�@Up�@U�@T��@T�/@Tj@SdZ@R�H@R�!@R�\@R=q@Qx�@QG�@QG�@Q7L@Q�@PĜ@PQ�@P1'@Pb@O��@O��@O�P@Ol�@O;d@N�y@N�@N��@M�@MO�@L��@L�@Lz�@LI�@L�@K��@K��@K�
@K��@KdZ@Ko@J�H@J�H@J�H@J��@J��@J��@J��@J�!@J�\@JM�@I��@I��@IG�@I%@HĜ@HbN@H �@G�;@G;d@F5?@D�D@C�m@C�@CdZ@C33@C@B�H@B~�@BM�@B=q@B=q@B�@A�7@A&�@@��@@��@@�@?�;@?�w@?K�@>�R@>V@>$�@=��@=�-@=O�@<��@<�j@<�@<j@<I�@;��@;ƨ@;��@;S�@:�@:��@:n�@:^5@:^5@:M�@:M�@:�@9��@9��@9�7@9x�@9X@9G�@97L@9&�@8��@8�9@8 �@7�@7��@7�P@7�P@7|�@7l�@7\)@6��@5@4��@49X@4(�@4�@4�@4�@4�@4�@4�@3�m@3ƨ@3dZ@3"�@2�H@2�!@2^5@2�@2J@1�#@1��@1�7@1&�@0�9@0r�@0A�@/�w@/|�@/\)@/+@/�@/
=@.��@.��@.�y@.ȴ@.��@.��@.��@.��@.V@.5?@-�T@,�/@,I�@+�@*��@*�@)�7@)x�@)X@)7L@)&�@)�@)%@(�`@(�9@(�@(A�@(  @'��@'�P@'K�@&�y@&�R@&�+@&V@&{@%p�@$��@$�/@$�/@$�@$�D@$z�@$Z@$�@$�@#��@#�
@#ƨ@#�F@#dZ@#@"�\@"n�@"=q@!��@!�@!�#@!�^@!x�@!&�@ ��@ r�@l�@�R@��@v�@5?@�T@@�@`B@�@�@V@�@��@I�@�m@�
@�F@�@C�@"�@o@�H@��@~�@-@-@�@��@��@x�@X@7L@�9@ �@  @�;@l�@K�@;d@+@+@
=@�R@ff@E�@5?@$�@{@{@{@{@{@{@@@�@��@�h@`B@/@�/@��@��@9X@(�@��@ƨ@��@t�@33@@�H@��@~�@n�@^5@M�@=q@=q@-@��@�7@�@��@�9@r�@A�@1'@b@  @  @�@�@�@�@��@�w@�@��@|�@;d@�@
=@�y@�@��@�+@v�@ff@ff@V@V@$�@�T@@@@�-@��@p�@O�@/@�/@�@Z@(�@�@�
@��@�@"�@o@@
�H@
��@
��@
��@
��@
�!@
��@
��@
��@
�\@
~�@
M�@
�@	�#@	��@	��@	x�@	�@��@�9@��@�u@r�@bN@Q�@ �@ �@ �@b@�;@�@|�@\)@
=@��@v�@E�@5?@$�@{@{@@�@@�@p�@�@�/@�/@z�@1@�F@�@dZ@C�@"�@��@��@~�@n�@^5@-@�@�#@��@��@�^@�^@��@��@�7@G�@ ��@ Ĝ@ ��@ Q�@ b?���?�\)?��?���?��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111YBVBVBVBPB+B!�B)�B'�B&�B%�B$�B!�B�B
=BB�B�HB�
BɺBĜB�XB�B�B��B��B��B�VB�Bt�Bk�BdZB^5BYBW
BQ�BL�BF�B?}B<jBC�BI�BF�BC�B=qB2-B.B'�B�BPBDB1BB�B��BǮB�FB��B� Bs�BjBO�B@�B"�B�B�B�BB
�mB
��B
ǮB
�}B
�-B
��B
�VB
t�B
^5B
K�B
?}B
;dB
1'B
!�B
�B
bB
+B	��B	�B	�B	�yB	�ZB	�BB	�#B	�B	��B	�^B	�-B	�!B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�uB	�PB	�B	w�B	o�B	m�B	jB	gmB	cTB	`BB	^5B	[#B	W
B	M�B	@�B	6FB	2-B	1'B	+B	%�B	%�B	"�B	�B	�B	�B	uB	\B	JB	DB	1B	B��B��B��B��B��B��B�B�`B�)B�
B��B��BŢB�jB�?B�9B�-B�!B�B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�JB�DB�DB�=B�7B�1B�+B�B�B�Bz�By�Bx�Bx�Bw�Bv�Bv�Bu�Bt�Bq�Bo�Bk�BjBiyBgmBdZBbNBbNBaHBaHB`BB_;B^5B[#BXBT�BP�BL�BJ�BH�BE�BC�BA�B?}B>wB=qB;dB:^B9XB8RB6FB49B2-B1'B/B.B,B+B)�B(�B'�B'�B&�B&�B&�B%�B$�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBoBoBoBhBhBbB\BbBbBbB\B\B\B\B\BhBhBoBoBoBuB{B�B�B�B�B�B�B�B�B �B!�B$�B)�B+B-B/B/B1'B2-B33B5?B6FB8RB8RB8RB=qB?}BB�BE�BH�BI�BI�BJ�BM�BQ�BS�BVBYB]/B_;B`BBaHBbNBcTBdZBe`BffBiyBiyBn�Bo�Br�Bt�Bv�B�B�B�+B�=B�PB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�9B�FB�FB�LB�LB�RB�RB�RB�XB�dB�qB�qB�}B�}B��B��B��BÖBÖBĜBĜBȴB��B��B��B��B��B��B�B�B�)B�HB�TB�fB�B�B��B��B��B��B��B��B	B	B	B	%B	%B	%B	1B		7B	
=B	
=B	DB	bB	�B	�B	�B	&�B	/B	5?B	8RB	8RB	8RB	8RB	8RB	8RB	8RB	8RB	8RB	8RB	8RB	9XB	:^B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	;dB	;dB	;dB	<jB	?}B	?}B	@�B	B�B	C�B	E�B	E�B	F�B	G�B	H�B	K�B	M�B	O�B	R�B	S�B	T�B	VB	VB	VB	VB	W
B	W
B	W
B	W
B	XB	ZB	[#B	\)B	^5B	^5B	`BB	dZB	ffB	ffB	iyB	l�B	o�B	p�B	q�B	r�B	t�B	u�B	w�B	y�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�1B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�?B	�FB	�LB	�LB	�XB	�dB	�dB	��B	ÖB	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�NB	�ZB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B

=B
DB
PB
VB
\B
\B
\B
bB
bB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
+B
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
.B
/B
/B
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
5?B
5?B
6FB
6FB
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
7LB
7LB
8RB
8RB
8RB
9XB
:^B
;dB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
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
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
O�B
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
S�B
S�B
T�B
T�B
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
XB
XB
XB
XB
XB
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
]/B
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
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
jB
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
n�B
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
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111YBpB�BBB.B}B%FB,B)yB(XB'B&fB%�B!�B�B
�B��B�,B�7B�^B�1B��B�iB�CB�B��B�_B�4B��Bv+Bl�Be�B_�BZBXBSBNVBHKB@�B<�BC�BJXBG�BEB>�B2�B/OB*0B�B�B�B	B%B�-B�4B��B��B� B��ButBm)BR�BC�B$tBBB�BSB
��B
�4B
��B
�UB
��B
��B
�:B
xlB
a-B
M�B
@�B
=<B
3�B
#TB
�B
�B
�B	�B	��B	�B	�B	�zB	�B	ܬB	�yB	�PB	��B	��B	��B	��B	�}B	�WB	�sB	�B	��B	��B	�/B	�dB	��B	�\B	��B	y>B	poB	nIB	kB	hsB	dB	`�B	^�B	\)B	Y1B	P}B	B�B	72B	33B	2|B	+�B	&LB	&�B	$tB	!�B	B	B	FB	�B	�B	�B		B	B��B��B�dB��B��B�FB�B�BݘB�1B��B��BȴB�B��B��B��B�[B��B��B��B��B��B��B�B�B��B�TB��B��B��B��B��B��B��B��B��B��B��B��B��B{�Bz*By$By$Bx8Bw2BwfBvzBu�Br�BqABk�BkQBj�Bh�BezBb�Bb�Ba�Ba�B`�B`'B_;B\]BYBV�BR�BM�BL0BJ=BF�BD�BB�B@�B?�B=�B<6B;dB9�B8�B7�B5�B3MB2B/�B/5B-B+�B*�B)�B(�B(>B'RB'8B'8B&LB%�B%FB#�B!bBB	B�B�B�B�B�B�B�B�B�ByBEBYBEB�B�B�B
BB9B�B�B�BB�B�B�B&BB�B�B�B�BB�B�B�BbB�B.BHB�B�B�B&B&B[BaBB9BSB�B�B�B	BIB �B!�B#TB&fB*�B+�B-�B/�B/�B1�B2�B3�B5�B6�B8�B8�B9rB>(B@OBCGBF?BIBJ	BJ=BKxBN�BR�BT�BV�BY�B]�B_�B`�Ba|Bb�Bc�Bd�Be�Bf�Bi�BjKBn�Bp!Bs3Bu�BxB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�HB�FB�XB�KB�qB��B��B��B�|B��B�`B�zB�fB�fB�lB�lB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�0B�JB�(B�NB�@BԕB�_BٴB��B�B�B�8B�B�B��B�B�B�0B�"B�qB	;B	aB	SB	?B	YB	tB	KB		RB	
XB	
rB	�B	�B	�B	#B	 \B	'mB	/�B	5ZB	8lB	8RB	8lB	8lB	8RB	8lB	8RB	8lB	8lB	8lB	8�B	9rB	:^B	:xB	:xB	:�B	:�B	:xB	:�B	:�B	;�B	;B	;�B	<�B	?�B	?�B	@�B	B�B	C�B	E�B	E�B	F�B	G�B	IB	LB	NB	PB	SB	T,B	UB	VB	VB	VB	VB	W
B	W?B	W$B	WYB	XEB	Z7B	[=B	\xB	^jB	^�B	`�B	d�B	f�B	f�B	i�B	l�B	o�B	p�B	q�B	r�B	t�B	vB	xB	zB	{B	|B	}"B	~(B	�4B	�'B	�-B	�-B	�3B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�$B	�=B	�=B	�WB	�]B	�;B	�3B	�tB	�FB	�LB	�fB	��B	��B	��B	��B	ðB	żB	żB	��B	�B	�B	�B	�B	�(B	�4B	�:B	�&B	�,B	�aB	�$B	�EB	�KB	�]B	�dB	ބB	ߊB	�pB	�vB	�|B	�bB	�|B	�bB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	��B	��B	��B	�	B	�$B	�>B	�*B	�B	�B	�B	�B	�.B
 B
  B
 B
;B
;B
'B
-B
3B
B
3B
B
3B
3B
3B
MB
MB
SB
YB
YB
?B
EB
fB
fB
	lB
	�B

�B
�B
�B
pB
vB
vB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#B
#�B
$B
$�B
$�B
$�B
$�B
$�B
%B
%�B
&2B
'B
(
B
'�B
'�B
'�B
(
B
($B
($B
(sB
+kB
-)B
.B
./B
.B
./B
.B
./B
./B
./B
./B
./B
/5B
/OB
0;B
1[B
1AB
2GB
2GB
2GB
3hB
3�B
3�B
4nB
5ZB
5�B
6`B
6`B
6`B
7fB
7LB
7fB
7LB
7fB
7fB
7fB
7LB
7LB
7�B
7fB
8�B
8�B
8�B
9�B
:�B
;�B
=�B
>�B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
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
J	B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
MB
MB
MB
N"B
O(B
O�B
Q B
Q B
Q B
RB
RB
RB
RB
SB
SB
S&B
SB
S&B
T,B
T,B
UB
UB
UB
U2B
UB
VB
VB
VB
VB
W
B
W$B
W?B
W$B
X+B
XEB
X+B
XEB
X_B
Y1B
Y1B
Z7B
Z7B
[#B
[=B
[#B
[WB
[=B
[=B
\CB
\CB
\)B
\CB
\)B
\)B
\)B
\CB
\CB
\)B
\CB
\]B
]IB
]IB
]dB
]dB
]dB
^OB
^jB
^OB
_;B
_VB
_pB
_pB
_VB
`vB
`vB
abB
a|B
abB
aHB
bhB
bNB
bhB
bNB
bhB
b�B
bhB
b�B
cnB
cnB
cnB
c�B
dtB
dtB
dZB
dZB
dtB
dZB
dZB
dZB
d�B
dZB
ezB
ezB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
ffB
f�B
gmB
gmB
g�B
g�B
g�B
gmB
g�B
gmB
gmB
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
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
n�B
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
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
tB
uB
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
xB
xB
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
zB
{B
{B
z�B
{B
{�B
|B
|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906130033072019061300330720190613003307202211182139112022111821391120221118213911201906140017422019061400174220190614001742  JA  ARFMdecpA19c                                                                20190603033726  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190602183843  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190602183845  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190602183845  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190602183846  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190602183846  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190602183846  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190602183846  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190602183847                      G�O�G�O�G�O�                JA  ARUP                                                                        20190602185714                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190602153044  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20190612153307  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190612153307  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190613151742  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123911  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                