CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-17T18:39:00Z creation;2020-02-17T18:39:02Z conversion to V3.1;2022-08-02T05:11:24Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200217183900  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               &A   JA  A30_8420_038                    2C  D   APEX                            8420                            2.11.2                          846 @��9�H 1   @���� @-Y��(�c�"��`1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�ffB�33B�  B�  B�  B�33B�33B�ffB�  B���B�  B�  B���B���B���B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  CL�C�fC�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF33CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @  @{�@�@���A=qA>�RA^�HA~�\A��A��A�33A�G�A�33A�p�A�p�A�
=B�B�RB��B�\B'��B/�
B7B?��BG��BO��BW�\B_�
Bg��Bo�Bw�B��B���B��)B���B���B���B���B���B���B��)B��HB��B��B�ǮB�B�33B�  B�ǮB��)B��
B�
=B��B�33B�ǮB߮B�qB��fB��B�3B�3B�B��B��fC�fC�C޸C޸C	�HC޸C�C�C�C��C�fC�3C+�C�=C�)C�HC!��C#��C%��C'��C)��C+�C-�C/��C1��C3�fC5��C7��C9�C;��C=��C?��CA�CC��CF#�CG��CI޸CK�HCM�fCO�fCQ�fCS�CU�CW�CY��C[�fC]��C_��Ca�HCc��Ce��Cg޸Ci��Ck��Cm��Co�fCq�Cs�Cu�3Cw�Cy��C{�fC}�fC��C���C��{C���C��RC��RC���C��
C��{C��3C���C��RC���C���C��
C��3C��3C��{C���C��\C��C��3C���C��
C���C��{C���C���C��{C���C��{C���C��\C���C��)C��)C��{C��{C���C��{C��{C��3C���C��3C���C��3C���C��C��3C��{C��3C���C���C��{C��{C���C��{C��
C��RC��3C��3C��{C��3C���C��3C��{C��
C��
C���C���C���C���C��
C��3C��C��
C��RC��
C��3C��C��
C��RC���C��{C���C���C���C���C���C��
C���C��RC��RC��
C��{C��
C��RC��
C���C���C��3C��C��3C��{C���C��
C���C��{C��3C���C��RC���C���C��3C���C��C��{C��3C��C��3C���C���C��
C��
C��RC��{C���C��
C���D {�D �)Dz=D�=Dz�D��Dz�D��D|)D��Dz=D��D|)D��Dz�D��D{�D��D	z�D	��D
z�D
��Dy�D��D{�D��D|)D�)D{�D��Dx�D��Dz=D��Dz=D��Dy�D�RDw
D�RDy�D��Dz�D��DxRD��DxRD�=D|)D��Dz=D�=Dz�D��D|�D��Dx�D�=Dz�D��Dz�D��D{�D��D y�D �RD!y�D!��D"y�D"�=D#|�D#�qD${�D$��D%z=D%��D&z�D&�)D'{�D'�=D(z=D(��D)z=D)�=D*z=D*��D+{�D+��D,{�D,��D-{�D-��D.x�D.��D/x�D/�RD0y�D0��D1xRD1�RD2z�D2�)D3|)D3��D4x�D4�=D5y�D5��D6}qD6��D7z=D7��D8z=D8��D9z�D9��D:z�D:�)D;{�D;�=D<z=D<�=D=z�D=�)D>z�D>�=D?z�D?��D@xRD@��DAy�DA��DB{�DB�=DCy�DC��DDw�DD��DE{�DE�)DF}qDF�)DGz�DG�=DHz=DH�=DIy�DI��DJ~DJ��DKy�DK�=DLz�DL��DM|)DM�)DN|)DN��DOx�DO��DP{�DP��DQ|�DQ��DR{�DR��DSy�DS��DTx�DT�=DUy�DU�RDVz=DV�)DW}qDW��DXz�DX��DYz�DY��DZ{�DZ��D[z�D[��D\xRD\��D]x�D]��D^z=D^�=D_x�D_�RD`x�D`��Daz=Da��DbxRDb�RDcx�Dc��Ddy�Dd��Dez=De��Dfy�Df��DgxRDg��Dhz�Dh��Diz=Di��Djz�Dj��Dkz�Dk�=Dlz=Dl��Dm{�Dm��DnxRDn��Doy�Do�RDpy�Dp��Dqy�Dq�RDrz=Dr��Ds|)Ds��Dtz=Dt��Du{�Du��Dvz=Dv��Dw|�Dw��Dxx�Dx��Dyy�Dy��Dz{�Dz�=D{x�D{�=D||)D|�)D}{�D}��D~z=D~��Dz�D��D�=D�}D���D��D�=qD�|�D��D��qD�=�D�~D��qD���D�=qD�}qD���D��)D�<{D�|)D���D��qD�=qD�~D��D��qD�=D�}D��qD���D�<{D�|{D���D���D�>fD�}D��{D��{D�=D�~D���D���D�=�D�}qD��qD���D�<)D�}D��D��{D�>D�
D��fD��qD�=D�|�D���D��D�=D�}D��D��qD�>fD�}qD��D��D�<�D�|�D��D���D�<)D�|{D��D��)D�=D�~D���D��D�<�D�|)D���D��3D�;�D�|{D���D���D�>D�~�D���D��)D�<)D�|�D��qD��qD�=qD�}�D��qD���D�>D�}�D��D���D�=qD�}D���D��{D�<�D�}�D��D���D�>D�}qD��D���D�=�D�}�D��fD��D�=qD�~D���D��)D�<{D�{�D��)D���D�<�D�}qD��D��qD�=D�|{D��{D���D�=�D�}D��D��D�<�D�|�D��qD���D�<{D�}qD���D��{D�;�D�}qD���D��D�=qD�|)D��{D��qD�=�D�}�D��fD���D�=D�|{D���D��\D�?
D�~D��qD��qD�<�D�|{D���D���D�<{D�}D��qD���D�=qD�}�D��qD���D�<)D�|)D���D��D�=qD�}qD��)D���D�=qD�}D��D��fD�>D�|{D���D��{D�<{D�|�D��qD��D�<�D�|�D��qD��D�=�D�}qD��D��qD�>fD�~�D��D��qD�>D�~D��D��)D�<{D�}D��{D���D�=D�}qD��fD���D�<�D�|�D��qD��qD�=qD�|�D��D��qD�=qD�~D��D��D�=D�}qD���D���D�<)D�}D��qD���D�=D�}qD��D��qD�>D�}qD��{D��D�=D�}qD��D��D�<{D�}qD���D���D�=qD�|{D���D��qD�=D�|�D��D���D�<�D�|)D»�D��{D�=�D�}qDü{D���D�=qD�~fDľD��D�>�D�}DŽqD��qD�<{D�|{DƽqD��D�=D�|�DǽqD��D�=D�|{DȻ�D��{D�=qD�|{DɽD��qD�=D�|)Dʻ�D���D�=�D�}�D˽D��)D�<�D�}qD̾D���D�<{D�}D;D���D�<�D�|)Dμ�D���D�;�D�|{DϽD��{D�>D�}�Dм{D��{D�<�D�}Dѽ�D��D�<�D�}�Dҽ�D��{D�<)D�{�Dӻ�D��)D�<)D�|�DԽqD��qD�<�D�{�Dռ{D��D�>fD�}�Dֽ�D��D�<�D�}�D׽�D���D�=�D�~�Dؽ�D���D�<)D�{�Dټ)D���D�<)D�{�Dڼ{D��D�=D�|�Dۼ�D���D�<{D�}Dܽ�D��D�=D�|)DݽD��D�=�D�}�D޾D��D�=�D�|�D߼�D��{D�=D�}�D�D��)D�<�D�}D�D��D�<{D�|)D��D���D�=D�}qD��D��qD�<{D�}D��D���D�=qD�|�D�D���D�=�D�}qD��D���D�=qD�}�D�D��D�=qD�|{D��D��{D�<{D�}D��D���D�<�D�|{D�{D���D�>D�|�D�{D��{D�<{D�}D�)D��3D�;�D�|�D���D���D�=D�}qD�D��D�<�D�}D�D��qD�>D�}qD�qD��D�=qD�~fD�qD��D�=D�|�D�qD��D�=D�|�D��D��D�<{D�~D��D��)D�;�D�}qD��D��qD�=�D�|{D��)D���D�=qD�~�D��fD���D�>�D�
D���D��{D�=qD�|{D���D��)D�=qD�~�D���D���D�=�D�a�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZQA�U�A�c�A�f2A�hsA�j�A�j�A�j�A�kA�j�A�j�A�jA�m�A�y�A�zDA�z�A�~�À4ÁA�zẢ7A̡-A�}�A�jA�VA�GA�1A���A��A��A��A�.�A�O�A�a�A�o�A�b�A�TaA�NpA�O�A�MA�>BA� 'A��A�MA�	�A��A�`BA��+A�f�A���A�]dA�%FA�{A��2A���A�g�A�$A�=A�3hA�$A���A�1A�&�A�&�A���A�6�A��JA���A�~A��A���AB�A{�-Az0UAw�VAt9�Ai��A^�hAZ-�AX�AVS&ATg�ARu�AN�AK��AJ2�AI]�AHaAD�)AB}VA@�[A?
�A;�"A:8A8�A7��A5�A5͟A5�A2�\A07LA-��A,
=A+^5A*��A)�+A(�A('�A&A�A${A `BAJ�AM�A��A�NA�A�A֡A��A�6A8�A=�A��A�HA9XA\�AzA��A;AOAdZA;dA��A�AHA��A�hAcA��A�SA|A<6A�A�:A33AG�A|Ah�AݘA�A�A��A��A-AMAA�A��A�$A�A�XAɆA6A�AP�A�A�A��A��A&A�A~(A
A
e�A	�A	�:A֡A�A�A��A��Aa�A��A�A�rAq�A=A�^A��ARTAOAn�AC-A��A��A+kA�TA�"A_A �dA 9�A �@��t@�dZ@�#�@��@�m]@�9X@��]@��f@�҉@�c�@�<�@���@��	@�V@��u@��[@�%@��[@��+@�ϫ@�*0@�J@�@�@�|�@�@�@�K�@�*0@��@�.�@�n@��@�1@��^@�%F@���@�Xy@�C-@�#:@�f@�j@�j@��@�֡@�\@�;d@��@�@�\)@�$@�M@�k@�<6@��@ߠ�@�q�@�<�@���@�ȴ@ۃ{@�!-@ڈ�@�1�@ي	@���@�a|@בh@��@�^5@ջ0@�E9@԰!@�:*@���@�Vm@�ߤ@Ғ�@�E�@���@�@�L0@�-@��@�<6@��@�Ov@�9X@��@�l�@�9�@��@�L0@�6@���@˥@ˀ4@�F@��c@ʺ�@�`�@�9X@��@ɉ7@��@Ȫe@�ff@�-�@���@�G�@��@���@��s@Ɓo@��@��d@�n/@�@@�w�@��)@Â�@�7L@´9@�J�@���@�O�@���@���@��h@�a|@���@��	@�\)@��}@��)@���@�}�@��@���@���@�q�@���@���@�a�@�!-@��	@���@�R�@�_@��@���@�:�@��F@�B[@�	�@��.@���@���@���@���@�j@�9X@��a@���@��{@��@��@���@�Z@���@�$t@��`@��}@�ff@��@��r@��)@���@��)@��.@�?�@��M@�8@�ی@��\@��@��@���@��@��~@��@��c@�&�@�p�@��@�
=@��@�g8@�Ta@�b@�t�@�F�@�#�@�ѷ@��r@�u�@�Ta@��}@�6z@��@�͟@��9@�kQ@��z@�K�@� \@��@��H@�E�@���@���@�RT@�)_@�	l@��@��@�U2@�7�@�	@���@�N<@�6z@��@��@��@���@�p;@�h�@�c @�Ta@�+k@��&@��X@�iD@�K�@��@���@�c�@�H@�3�@���@���@��4@��@�{�@�0U@�$�@�4@��9@���@��V@��@�\)@�/@���@��1@��@� �@�"�@��y@��+@�-�@�_@��o@���@��F@���@��@���@�s@�`B@��@��@�J�@�	@��@���@���@�]�@��L@�Z@�+k@�b@���@��@�}�@�o@��@�`�@���@�l�@�K�@�5�@�#�@���@���@���@�s�@�U2@�5?@��@���@���@�e�@�X@�*0@��/@��A@�7�@�u@���@��h@��M@�k�@�/�@��)@��z@��_@���@�C�@��@���@���@�m]@��@�͟@��@���@���@���@���@�dZ@��P@���@�Q�@��A@���@��@�6z@�!-@��8@�u%@�A�@�b@��z@�e,@�N<@�0�@��@�֡@��Y@�L0@��@��@��*@��@�S&@�@��@���@��@�M@�$@���@���@��@�n/@�s�@�Vm@�#�@��5@�Ɇ@���@��@�d�@�[@C�@~�@~�6@~W�@~E�@~�@}�S@|��@|��@|��@|%�@{y�@{S�@{�@z�]@z��@z^5@zC�@z{@yԕ@y�@yX@yQ�@y2a@x`�@x�@xG@w�:@v�8@v��@u��@uQ�@t֡@t�@s�@r�F@r1�@q�)@q�n@q�@p��@pc�@o˒@o9�@n�@nR�@m�@m�X@m}�@lѷ@k��@k��@k��@ks@j��@jM�@i�@i�^@i��@i}�@i#�@h�?@hr�@g;d@g�@f�y@fff@e�-@d��@d�@c_p@b{�@b �@a�@a�Z@a�@a�@`�@`�@`h�@`:�@`�@`@_خ@_�P@_33@^�M@^�@^�}@^YK@]�)@]��@]Dg@]@\��@\�.@[�r@[s@[X�@[o@Z�<@Z�\@Z\�@Z)�@Y��@Y�3@YY�@Y�@X�_@XU2@X?�@X4n@X�@Wƨ@Wa@V�s@V��@V�A@V.�@U��@UY�@UV@T��@T�@S��@SK�@S�@R�B@R�r@R\�@Q�#@Qc@Q`B@QL�@Q+�@Q�@P�O@P-�@O�]@O�}@O˒@O��@O�{@O/�@N��@N��@N#:@M��@M�@MB�@L��@L�4@K�@Kx@J�@J��@J=q@I��@H��@H �@G��@GK�@F�@F@�@E��@E�@Du�@DM@C�P@B҉@Bz@B?@B�@A�@AV@@M@@�@@@@�@?��@?��@?��@?s@?O@?�@>#:@=��@=c@<�U@<�4@<��@<~(@<'R@<@;�&@;!-@:�A@:{@9�d@9�@8�4@8[�@7�]@7�[@7��@7@6��@6�6@6u%@5�@5�@5s�@50�@5�@4�[@4m�@4  @3��@3��@3�6@3��@3dZ@3@O@2��@2�@2�r@2W�@2)�@2@1�T@1�-@1\�@1�@0��@0�@0w�@0A�@/��@/��@/��@/a@/9�@/�@/�@.�@.��@.Z�@.V@.Ta@.)�@.@-��@-�~@-7L@-@,��@,��@,�4@,�D@,6@,�@+� @+K�@+�@*��@*�@*� @*@)��@(��@(�_@(�@(m�@(PH@(,=@(	�@'�6@'�	@'!-@'(@&�@&5?@%�D@%��@%��@%�@%w2@%:�@$�@$z�@$tT@$[�@$<�@#�W@#�k@#6z@"�@"�@"_�@"�@!@!�"@!^�@!q@ �|@ �U@ ~(@ ~@��@�$@=@��@�@�m@q�@B[@�@�=@rG@=�@�@�Y@"h@��@�q@��@e�@K�@@ȴ@a|@_@��@T�@�@�O@_@(�@G@��@E9@�@�B@��@s�@W�@�@�j@�-@zx@`B@	l@�e@��@C-@G@�
@�0@�*@��@j�@8@(@�y@��@��@}V@i�@V@8�@�@��@�@��@�h@u�@<6@#�@�@�?@�.@]d@6@7@  @�K@�k@j�@P�@A�@C@��@�y@�]@��@��@�@p;@J�@5?@�@_@�D@�)@�j@��@�C@��@N<@�@��@�Y@bN@Q�@7�@�@��@�	@�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZQA�U�A�c�A�f2A�hsA�j�A�j�A�j�A�kA�j�A�j�A�jA�m�A�y�A�zDA�z�A�~�À4ÁA�zẢ7A̡-A�}�A�jA�VA�GA�1A���A��A��A��A�.�A�O�A�a�A�o�A�b�A�TaA�NpA�O�A�MA�>BA� 'A��A�MA�	�A��A�`BA��+A�f�A���A�]dA�%FA�{A��2A���A�g�A�$A�=A�3hA�$A���A�1A�&�A�&�A���A�6�A��JA���A�~A��A���AB�A{�-Az0UAw�VAt9�Ai��A^�hAZ-�AX�AVS&ATg�ARu�AN�AK��AJ2�AI]�AHaAD�)AB}VA@�[A?
�A;�"A:8A8�A7��A5�A5͟A5�A2�\A07LA-��A,
=A+^5A*��A)�+A(�A('�A&A�A${A `BAJ�AM�A��A�NA�A�A֡A��A�6A8�A=�A��A�HA9XA\�AzA��A;AOAdZA;dA��A�AHA��A�hAcA��A�SA|A<6A�A�:A33AG�A|Ah�AݘA�A�A��A��A-AMAA�A��A�$A�A�XAɆA6A�AP�A�A�A��A��A&A�A~(A
A
e�A	�A	�:A֡A�A�A��A��Aa�A��A�A�rAq�A=A�^A��ARTAOAn�AC-A��A��A+kA�TA�"A_A �dA 9�A �@��t@�dZ@�#�@��@�m]@�9X@��]@��f@�҉@�c�@�<�@���@��	@�V@��u@��[@�%@��[@��+@�ϫ@�*0@�J@�@�@�|�@�@�@�K�@�*0@��@�.�@�n@��@�1@��^@�%F@���@�Xy@�C-@�#:@�f@�j@�j@��@�֡@�\@�;d@��@�@�\)@�$@�M@�k@�<6@��@ߠ�@�q�@�<�@���@�ȴ@ۃ{@�!-@ڈ�@�1�@ي	@���@�a|@בh@��@�^5@ջ0@�E9@԰!@�:*@���@�Vm@�ߤ@Ғ�@�E�@���@�@�L0@�-@��@�<6@��@�Ov@�9X@��@�l�@�9�@��@�L0@�6@���@˥@ˀ4@�F@��c@ʺ�@�`�@�9X@��@ɉ7@��@Ȫe@�ff@�-�@���@�G�@��@���@��s@Ɓo@��@��d@�n/@�@@�w�@��)@Â�@�7L@´9@�J�@���@�O�@���@���@��h@�a|@���@��	@�\)@��}@��)@���@�}�@��@���@���@�q�@���@���@�a�@�!-@��	@���@�R�@�_@��@���@�:�@��F@�B[@�	�@��.@���@���@���@���@�j@�9X@��a@���@��{@��@��@���@�Z@���@�$t@��`@��}@�ff@��@��r@��)@���@��)@��.@�?�@��M@�8@�ی@��\@��@��@���@��@��~@��@��c@�&�@�p�@��@�
=@��@�g8@�Ta@�b@�t�@�F�@�#�@�ѷ@��r@�u�@�Ta@��}@�6z@��@�͟@��9@�kQ@��z@�K�@� \@��@��H@�E�@���@���@�RT@�)_@�	l@��@��@�U2@�7�@�	@���@�N<@�6z@��@��@��@���@�p;@�h�@�c @�Ta@�+k@��&@��X@�iD@�K�@��@���@�c�@�H@�3�@���@���@��4@��@�{�@�0U@�$�@�4@��9@���@��V@��@�\)@�/@���@��1@��@� �@�"�@��y@��+@�-�@�_@��o@���@��F@���@��@���@�s@�`B@��@��@�J�@�	@��@���@���@�]�@��L@�Z@�+k@�b@���@��@�}�@�o@��@�`�@���@�l�@�K�@�5�@�#�@���@���@���@�s�@�U2@�5?@��@���@���@�e�@�X@�*0@��/@��A@�7�@�u@���@��h@��M@�k�@�/�@��)@��z@��_@���@�C�@��@���@���@�m]@��@�͟@��@���@���@���@���@�dZ@��P@���@�Q�@��A@���@��@�6z@�!-@��8@�u%@�A�@�b@��z@�e,@�N<@�0�@��@�֡@��Y@�L0@��@��@��*@��@�S&@�@��@���@��@�M@�$@���@���@��@�n/@�s�@�Vm@�#�@��5@�Ɇ@���@��@�d�@�[@C�@~�@~�6@~W�@~E�@~�@}�S@|��@|��@|��@|%�@{y�@{S�@{�@z�]@z��@z^5@zC�@z{@yԕ@y�@yX@yQ�@y2a@x`�@x�@xG@w�:@v�8@v��@u��@uQ�@t֡@t�@s�@r�F@r1�@q�)@q�n@q�@p��@pc�@o˒@o9�@n�@nR�@m�@m�X@m}�@lѷ@k��@k��@k��@ks@j��@jM�@i�@i�^@i��@i}�@i#�@h�?@hr�@g;d@g�@f�y@fff@e�-@d��@d�@c_p@b{�@b �@a�@a�Z@a�@a�@`�@`�@`h�@`:�@`�@`@_خ@_�P@_33@^�M@^�@^�}@^YK@]�)@]��@]Dg@]@\��@\�.@[�r@[s@[X�@[o@Z�<@Z�\@Z\�@Z)�@Y��@Y�3@YY�@Y�@X�_@XU2@X?�@X4n@X�@Wƨ@Wa@V�s@V��@V�A@V.�@U��@UY�@UV@T��@T�@S��@SK�@S�@R�B@R�r@R\�@Q�#@Qc@Q`B@QL�@Q+�@Q�@P�O@P-�@O�]@O�}@O˒@O��@O�{@O/�@N��@N��@N#:@M��@M�@MB�@L��@L�4@K�@Kx@J�@J��@J=q@I��@H��@H �@G��@GK�@F�@F@�@E��@E�@Du�@DM@C�P@B҉@Bz@B?@B�@A�@AV@@M@@�@@@@�@?��@?��@?��@?s@?O@?�@>#:@=��@=c@<�U@<�4@<��@<~(@<'R@<@;�&@;!-@:�A@:{@9�d@9�@8�4@8[�@7�]@7�[@7��@7@6��@6�6@6u%@5�@5�@5s�@50�@5�@4�[@4m�@4  @3��@3��@3�6@3��@3dZ@3@O@2��@2�@2�r@2W�@2)�@2@1�T@1�-@1\�@1�@0��@0�@0w�@0A�@/��@/��@/��@/a@/9�@/�@/�@.�@.��@.Z�@.V@.Ta@.)�@.@-��@-�~@-7L@-@,��@,��@,�4@,�D@,6@,�@+� @+K�@+�@*��@*�@*� @*@)��@(��@(�_@(�@(m�@(PH@(,=@(	�@'�6@'�	@'!-@'(@&�@&5?@%�D@%��@%��@%�@%w2@%:�@$�@$z�@$tT@$[�@$<�@#�W@#�k@#6z@"�@"�@"_�@"�@!@!�"@!^�@!q@ �|@ �U@ ~(@ ~@��@�$@=@��@�@�m@q�@B[@�@�=@rG@=�@�@�Y@"h@��@�q@��@e�@K�@@ȴ@a|@_@��@T�@�@�O@_@(�@G@��@E9@�@�B@��@s�@W�@�@�j@�-@zx@`B@	l@�e@��@C-@G@�
@�0@�*@��@j�@8@(@�y@��@��@}V@i�@V@8�@�@��@�@��@�h@u�@<6@#�@�@�?@�.@]d@6@7@  @�K@�k@j�@P�@A�@C@��@�y@�]@��@��@�@p;@J�@5?@�@_@�D@�)@�j@��@�C@��@N<@�@��@�Y@bN@Q�@7�@�@��@�	@�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�{BԯB��B�MB��B�BՁB��BՁBՁBԯB�
B�IBݘB�~B�\B��B�B�B�HB�<B�1B�KB��B�BۦB�NB��B�	B�BPB
B�B!-B vB �B"4B%zB)_B*�B+�B/B33BC-BH�B	I�B
�B
��B
�~B
�rB
ǔB
��B
�|B
�hB
�B
�B
��B
�dB
��B
�>B
�YB
�eB
�RB
�vBB
�B
|B
-�B
{B	ʌB	�2B	��B	�-B	z�B	mB	CB	$B	�B�B��B�B�B�B��B��B��B�CB�6B�'B��B��B��B	 �B	�B	�B		RB		RB	
B	B	NB	�B��B��B�^B	�B	2B	FB	xB	B�9B�B�}B	�B	�B	OB	3�B	FYB	6�B	7LB	B�B	YeB	f�B	l"B	y�B	�oB	�B	|jB	��B	�KB	�,B	�OB	�QB	��B	��B	��B	��B	��B	�OB	�OB	�4B	�OB	�B	��B	��B	ǔB	�1B	�0B	��B	�hB	��B	ΊB	ϫB	��B	��B	��B	�OB	�5B	��B	��B	�B	�}B	�BB	�:B	��B	��B	�pB	��B	�BB	��B	�|B	�dB	ݘB	��B	��B	ܬB	�qB	�+B	��B	�sB	ٚB	خB	��B	՛B	�MB	�YB	�2B	�2B	�B	�
B	ܬB	��B	��B	�]B	ۦB	��B	��B	�
B	՛B	�,B	ևB	�SB	��B	�B	ԯB	��B	�&B	�&B	ӏB	�&B	�uB	�FB	�FB	��B	�&B	��B	�@B	�uB	�uB	�B	ԕB	�B	�B	�MB	�B	��B	�MB	��B	ԕB	՛B	յB	յB	��B	��B	�sB	�sB	��B	�B	�+B	��B	רB	�YB	�$B	��B	ԯB	�@B	�B	ӏB	�,B	ՁB	�,B	ӏB	��B	�oB	�[B	өB	��B	��B	�2B	��B	�,B	�B	�B	��B	��B	�+B	��B	ٴB	�kB	��B	��B	یB	�WB	��B	��B	�xB	�B	�B	�/B	�/B	�~B	ݘB	�OB	�B	��B	��B	�\B	�B	��B	�B	�B	��B	�B	�B	�B	��B	�B	� B	�B	�B	�B	�ZB	�B	��B	�B	�B	�
B	�$B	�$B	�>B	�XB	�sB	�XB	�>B	�B	��B	��B	�B	�B	��B	�B	�6B	�B	�=B	��B	�B	�cB	��B	��B	� B	��B	�B	��B	�AB	�B	��B	�B	��B	�|B	��B	��B	�3B	��B	�TB	�nB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	�B	�RB	��B	�rB	��B	�rB	��B	�xB	�xB	��B	��B	�B	��B	��B	��B	��B	�wB	��B	��B
 �B
B
 �B
 �B
�B
�B
aB
3B
�B
MB
B
�B
�B
�B
�B
�B
	RB

XB
�B
1B
�B
	B
	�B
�B
	�B
DB

XB
	�B
	B
	B
xB
JB
�B
~B
�B
B
�B
�B
vB
�B
B
HB
�B
B
:B
hB
B
NB
4B
 B
�B
�B
oB
�B
�B
B
B
[B
aB
,B
�B
�B
�B
�B
�B
B
�B
�B
mB
�B
�B
sB
�B
YB
�B
+B
+B
_B
yB
�B
yB
�B
EB
�B
7B
7B
7B
7B
7B
B
	B
�B
�B
QB
�B
eB
�B
_B
�B
�B
�B
EB
#B
�B
�B
IB
IB
�B
]B
jB
�B
jB
�B
~B
�B
B
�B
�B
�B
B
B
pB
�B
B
�B
�B
pB
�B
 \B
 �B
!|B
!|B
!�B
"�B
"�B
# B
$�B
$�B
$�B
%B
%,B
%FB
%,B
$�B
$�B
$�B
%`B
%�B
&�B
'RB
'mB
(XB
(sB
(XB
(�B
*B
)�B
*0B
*eB
*�B
+6B
+B
*�B
+B
*�B
*�B
*eB
*B
)�B
(�B
(>B
'�B
'�B
'�B
'�B
'�B
'�B
)B
)B
)�B
)�B
*�B
*�B
*�B
+B
+QB
+�B
+�B
,�B
,�B
,�B
,�B
,�B
-CB
-wB
-�B
-]B
.IB
.B
.�B
.�B
/B
/�B
0�B
1�B
2aB
2|B
2aB
2aB
2aB
2�B
3�B
3�B
3�B
3�B
3�B
4B
49B
4�B
5?B
5%B
5%B
5tB
5�B
5�B
5�B
6B
6FB
6FB
6`B
6zB
6�B
7B
7fB
8B
7�B
8B
8B
7�B
8B
8lB
8�B
9$B
9$B
9XB
9�B
:B
:DB
:�B
:�B
:�B
;B
:�B
;JB
<B
<PB
<6B
<�B
=�B
>B
>wB
>�B
>�B
?HB
?}B
?}B
?�B
@B
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
A�B
B[B
B�B
CB
CGB
CGB
CGB
C�B
CaB
CGB
C-B
C-B
C{B
C�B
C�B
C�B
C�B
C�B
DB
D�B
EB
ESB
ESB
EmB
E�B
E�B
E�B
FB
FYB
FtB
FtB
F�B
G_B
G�B
G�B
HB
H1B
H1B
HKB
HKB
HfB
HfB
H�B
H�B
IB
I7B
IRB
IRB
IRB
IlB
I�B
J#B
JrB
JrB
J�B
KB
J�B
J�B
KxB
KDB
K)B
KDB
KDB
KxB
K�B
K�B
L0B
L~B
L~B
L~B
L�B
L~B
MB
L�B
M6B
MjB
MPB
MPB
M�B
N�B
N�B
N�B
O\B
O\B
OvB
P}B
P.B
PbB
P�B
Q B
Q4B
Q�B
Q�B
RB
R�B
S�B
S�B
TFB
T�B
U�B
U�B
U�B
V9B
VSB
WYB
X_B
XyB
X�B
X�B
X�B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
YB
Y1B
YKB
Y�B
Y�B
X�B
YeB
Z7B
Y�B
Y�B
ZB
[#B
[	B
Z�B
Z�B
Z�B
[	B
[=B
[qB
[�B
[�B
\CB
\CB
\CB
\�B
\�B
\�B
\�B
]dB
]dB
]~B
]~B
]�B
]�B
^5B
^�B
^�B
^jB
^�B
^�B
_B
_!B
_!B
_pB
_�B
_�B
_�B
_�B
_�B
`BB
`BB
`\B
`�B
`�B
`�B
`�B
aB
aHB
a|B
a�B
a�B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
c:B
cnB
c�B
c�B
c�B
c�B
d&B
dZB
dtB
dZB
d�B
eFB
f2B
f2B
fB
ffB
ffB
ffB
f�B
f�B
f�B
g�B
g�B
hsB
hXB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i*B
i_B
jB
j0B
j0B
j�B
j�B
jB
j�B
k6B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
m)B
mwB
m�B
m�B
n/B
ncB
n�B
n�B
oOB
oOB
oiB
o�B
o�B
pB
p;B
p;B
pUB
p�B
qB
qvB
q�B
q�B
q�B
rB
rB
r-B
raB
r�B
sB
s�B
s�B
tB
tB
t�B
t�B
t�B
u?B
uZB
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
xB
x8B
x8B
xRB
x�B
x�B
x�B
x�B
y	B
y>B
y�B
y�B
y�B
y�B
y�B
z*B
z*B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
{B
{0B
{dB
{B
{B
{�B
{�B
|B
|PB
|PB
|jB
|�B
|�B
|�B
}"B
}VB
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~(B
~BB
~wB
~�B
~�B
~�B
.B
cB
�B
�B
�4B
�4B
�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�aBԯB��B�MB��B�BՁB��BՁBՁBԯB�$B�/BݘBݘB�\B��B�QBOB��B��B�KB�B�1B�kB��B�hB��B��BoBB�B�B!bB �B!B"NB%�B)yB*�B,"B/�B4�BG�BX�B	Z�B
�B
��B
��B
�"B
͹B
�zB
�9B
��B
��B
ؓB
�B
�B
�B
��B
��B
�B
�B
��BKB
�B
�tB
5tB
 BB	�(B	�0B	��B	�_B	�B	x�B	OvB	�B	�B��B��B��B�TB�B��B�pB�OB�vB�/B�B��B�.B	B	�B	�B	zB		�B	B	�B	 BB	aB	�B��B�B�B	B	�B	$B	�B	9B��B�<B	 B	�B	�B	B	4nB	HfB	7�B	6�B	A�B	X�B	ffB	k�B	y�B	��B	��B	|�B	�{B	�KB	�{B	��B	��B	��B	��B	�2B	�0B	�wB	��B	��B	��B	��B	��B	B	�B	�fB	ȀB	��B	уB	�:B	ЗB	ϫB	�.B	��B	�B	�xB	��B	�VB	ߊB	�KB	��B	�4B	��B	ҽB	�aB	�EB	�B	�pB	�B	�B	�B	�B	�OB	ݲB	��B	��B	�xB	��B	�_B	��B	ڠB	�B	�eB	��B	��B	��B	�gB	յB	�9B	�
B	�B	�dB	�IB	��B	�)B	�qB	ڠB	׍B	�mB	ԕB	ּB	֡B	�B	՛B	՛B	��B	ӏB	өB	��B	ӏB	��B	ԯB	ԕB	�,B	ӏB	�aB	өB	��B	��B	ԕB	�B	��B	�mB	��B	�mB	�SB	յB	��B	��B	�B	�9B	�9B	ևB	�?B	��B	��B	�EB	�_B	�_B	�_B	�EB	�_B	�+B	ԕB	�gB	��B	��B	��B	��B	�SB	��B	��B	�:B	��B	��B	�aB	ԯB	�gB	յB	՛B	��B	�SB	�mB	�?B	�sB	خB	�eB	�QB	��B	�=B	�]B	��B	��B	�B	�B	��B	�dB	�~B	�~B	ݲB	�B	�B	ބB	�OB	ބB	�\B	�B	�BB	�\B	��B	��B	�4B	��B	��B	��B	�B	�:B	�TB	��B	��B	��B	�B	��B	�`B	�B	�RB	�XB	�XB	�B	�B	�B	�B	�B	�B	��B	�DB	�B	�_B	�yB	�B	��B	�B	�=B	�B	�B	�B	�B	�/B	��B	�OB	��B	�AB	�'B	��B	�B	�B	��B	�-B	�B	�B	�3B	�B	�9B	�B	��B	��B	�%B	��B	�+B	�+B	�`B	�2B	�8B	�RB	�RB	�B	�8B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	�0B	��B	�B	�<B	�(B	��B	��B	�.B	��B
 �B
 B
UB
oB
�B
�B
�B
�B
B
�B
gB
�B
�B
�B
�B
�B
	�B

�B
	B
�B
	B
	7B

#B
�B
	�B
�B

�B
	�B
	7B
	7B
�B
�B
B
B
JB
6B
�B
B
�B
.B
HB
}B
�B
oB
�B
�B
NB
hB
NB
4B
 B
 B
�B
,B
2B
FB
&B
�B
�B
FB
,B
�B
�B
2B
B
MB
�B
B
�B
�B
$B
�B
+B
�B
B
_B
yB
�B
�B
eB
�B
�B
yB
B
kB
QB
kB
�B
�B
�B
#B
#B
�B
�B
B
�B
1B
yB
B
�B
B
EB
WB
B
B
dB
�B
dB
�B
�B
�B
�B
5B
�B
!B
;B
�B
�B
B
;B
VB
�B
�B
�B
B
�B
�B
�B
 vB
 �B
!�B
!�B
"4B
#B
# B
#�B
$�B
$�B
%B
%FB
%`B
%�B
%�B
$�B
$�B
$�B
%zB
%�B
&�B
'�B
'�B
(�B
(�B
(sB
(�B
*KB
*B
*B
*�B
+QB
+�B
+QB
+kB
+kB
*�B
*�B
*�B
*eB
*B
(�B
(�B
(
B
'�B
'�B
(
B
(
B
(>B
)_B
)DB
)�B
*KB
*�B
*�B
+B
+QB
+�B
+�B
+�B
,�B
,�B
-B
,�B
,�B
-]B
-�B
-�B
-�B
.cB
.IB
.�B
.�B
/OB
/�B
0�B
2B
2�B
2�B
2|B
2�B
2�B
3B
4B
4B
4B
4B
4B
49B
4�B
5ZB
5tB
5?B
5ZB
5�B
5�B
5�B
5�B
6FB
6`B
6`B
6zB
6�B
6�B
72B
7�B
8B
8B
8RB
88B
8B
8RB
8�B
8�B
9XB
9XB
9�B
9�B
:^B
:�B
:�B
:�B
:�B
;0B
;B
;B
<PB
<�B
<�B
<�B
=�B
>(B
>�B
>�B
>�B
?cB
?�B
?�B
@B
@OB
@�B
@�B
@�B
AUB
A�B
A�B
B'B
A�B
BB
B�B
B�B
C{B
C{B
C�B
C�B
C�B
CaB
CaB
CGB
C{B
C�B
C�B
C�B
C�B
DB
C�B
D3B
D�B
EB
EmB
EmB
E�B
E�B
FB
FB
F?B
F�B
F�B
F�B
F�B
G�B
H1B
HB
H1B
HfB
HKB
HfB
HfB
H�B
H�B
H�B
H�B
IRB
I7B
IlB
IlB
I�B
I�B
J	B
J=B
J�B
J�B
KB
KDB
K)B
K)B
K�B
K^B
K^B
K^B
K^B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
L�B
L�B
MPB
MB
MPB
M�B
M�B
MjB
M�B
OB
OB
N�B
OvB
O�B
O�B
P�B
PHB
P�B
P�B
Q4B
QhB
Q�B
R B
RoB
S@B
S�B
TB
T{B
T�B
U�B
VB
U�B
VmB
V�B
W�B
X�B
X�B
X�B
X�B
YB
X�B
X�B
X�B
X�B
X�B
Y1B
YB
YKB
YKB
YB
ZB
ZB
YB
Y�B
ZQB
Y�B
Y�B
Z7B
[WB
[#B
[	B
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\xB
\xB
\�B
\�B
\�B
\�B
]/B
]�B
]~B
]�B
]�B
]�B
^B
^OB
^�B
^�B
^�B
^�B
^�B
_!B
_;B
_;B
_�B
_�B
_�B
_�B
`B
`'B
`\B
`\B
`vB
`�B
`�B
`�B
aB
aHB
abB
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
cB
cB
b�B
b�B
c:B
cnB
c�B
c�B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
ezB
ffB
ffB
fLB
f�B
f�B
ffB
f�B
f�B
gB
g�B
g�B
h�B
hsB
iB
iDB
h�B
h�B
h�B
h�B
iyB
iDB
i�B
jKB
jKB
jKB
j�B
j�B
j�B
kB
k�B
k�B
k�B
lB
lWB
l�B
l�B
mB
mCB
m�B
m�B
m�B
nIB
n�B
n�B
o B
oiB
oiB
o�B
o�B
o�B
p;B
poB
pUB
poB
p�B
q'B
q�B
q�B
q�B
r-B
r-B
rGB
raB
r�B
r�B
sMB
s�B
s�B
t9B
tTB
t�B
t�B
t�B
uZB
utB
u�B
u�B
vB
vFB
v`B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
x8B
xRB
x8B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{JB
{B
{�B
{�B
{�B
|B
|B
|jB
|jB
|�B
|�B
|�B
|�B
}VB
}qB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~(B
~BB
~]B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�iB
�OB
�43311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<|Q�<���<AT�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<'�<#�
<<j<#�
<#�
<#�
<#�
<#�
<<j<F?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002290102472020022901024720200229010247202207271134492022072711344920220727113449202207271537122022072715371220220727153712  JA  ARFMdecpA30a                                                                20200217183747  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200217183900  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200217183900  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200217183901  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200217183901  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200217183901  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200217183902                      G�O�G�O�G�O�                JA  ARUP                                                                        20200217185400                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200219000000  CF  PSAL_ADJUSTED_QC@@���G�O�                JM  ARCAJMQC2.0                                                                 20200228160247  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200228160247  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023449  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063712  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                