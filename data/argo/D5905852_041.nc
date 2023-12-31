CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-03-19T00:37:56Z creation;2020-03-19T00:37:58Z conversion to V3.1;2022-08-02T05:11:16Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200319003756  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               )A   JA  A30_8420_041                    2C  D   APEX                            8420                            2.11.2                          846 @�5�n��1   @�6W:� @/J0U2a|�c)�z�H1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B㙚B�  B�ffBB�  B�  B�  C   C  C  C  C  C
  C  C  C�3C��C�fC  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z�@~{@��@��RA\)A?\)A`Q�A�{A��A���A�A�Aϙ�A�A�A�B  B  B�B�
B'�
B/�B7��B?�
BG�RBO�HBW�
B_�Bg�HBo��Bw�
B�HB��B��fB��B��HB���B��)B��B�\B�  B��3B�ǮB�B��HB��B��B��B���B��)B���B��fB��HB��B���B�  B�{B���B�B�B� B���B��HB��fB��C�C�C�3C��C	�RC��C��C�)C�\C��C�3C��C�3C�C�C�3C!��C#�3C&�C'�qC)�3C+��C-��C/�C1��C3�RC5�C7�C9�C;�C=�C?�3CA��CC�CE�3CG��CI��CK��CN�CO�qCQ�CS�CU��CW�RCY�RC[�qC]�qC`  Cb  Cc�3Ce��Cg�Ci�Ck�Cm�Co�Cq�3Cs�3Cu��Cw�RCy�3C{�C}��C�C���C���C���C���C��qC���C���C��RC��
C��RC���C��qC���C��RC��
C��RC���C���C��RC��)C���C��qC��RC���C��)C��)C���C��
C���C��qC��qC���C��C�HC���C���C���C���C���C���C��)C��RC���C��)C���C��
C��RC���C��RC���C��)C��RC��RC��)C��qC���C���C���C��)C���C���C���C���C��)C���C��
C���C���C���C��)C���C��qC���C���C���C��qC���C��)C��)C��qC���C���C�  C��)C��
C��RC���C��qC���C��RC���C�  C��)C��RC���C��)C���C�  C��C��)C���C��)C��)C��RC���C��)C���C���C���C��)C���C���C��RC���C��
C��
C���C��)C��)C��RC���C���C��
C���C���C��RC���C��RD |�D �qD{�D�D}qD��D|)D�D~D�qD}qD�=Dz=D��D|�D��D|�D�D	~D	�D
|�D
�qD~�D��D~D��D}qD��D|�D�qD~D�)Dz=D��D|)D��D}qD�)D|�D��D|)D��D~D��D|)D��D}qD��D}qD�qD}qD�qD~D��D}qD�D� D�\D~D�D{�D��D|�D�D � D �\D!~�D!�D"~�D"�)D#z�D#��D$|)D$��D%}qD&  D&~D&��D'|)D'�D(~D(�qD)|)D)�qD*}qD*��D+}qD+�qD,~D,�qD-}qD-�qD.}qD.�qD/~�D/��D0~�D0�qD1}qD1��D2~�D2�D3~D3��D4|�D4��D5|�D5�D6~�D6�\D7~�D7��D8}qD8�D9|�D9�)D:}qD:�D;|�D;��D<}qD<��D=|)D=��D>~D>�\D?~D?�qD@}qD@�qDA~DA��DB|�DB�)DC|)DC�)DD}qDD�DE\DE�DF~DF��DG|)DG�)DH|�DH�qDI}qDI��DJ\DJ�\DK\DK�\DL��DL�DM|)DM�DN\DO �DO~�DO��DP~�DP�qDQ|�DQ�)DR}qDR�\DS~DS��DT}qDT�qDU~�DU�DV|�DV��DW}qDW��DX}qDX�DY~DY�DZ|�DZ�)D[|�D[��D\|)D\�qD]\D]�D^~D^�D_|�D_�)D`|)D`��Daz=Da�)Db}qDb�qDc|)Dc�)Dd~De  De~De��Df~�Df��Dgz�Dg��Dh~Dh��Di~Di�Dj\Dj�\Dk\Dk�\Dl|�Dl�)Dm|�Dm��Dn|�Dn�Do~Do�qDp}qDp�qDq~Dq��Drz�Dr�qDs|�Ds�=Dtz�Dt��Du|�Du�Dv}qDv��Dwz�Dw��Dx\Dx��Dy}qDy��Dz|�Dz��D{}qD{�qD||�D|��D}~�D}�D~~�D~�qD{�D�)D�?
D�~�D��\D��
D�?\D��D��
D���D�=�D�~D���D���D�>�D�
D��\D��
D�>fD�
D��
D���D�>D�~fD��fD��\D�?�D�\D��fD��D�>�D�~fD���D��\D�>�D�
D���D���D�?
D�
D��\D��\D�?\D��RD���D���D�?�D�
D��fD��fD�?
D�~fD��fD��fD�?
D�~fD��D���D�@ D�~�D��qD��D�>�D�
D��qD���D�>D�~D��fD��
D�@ D��D��
D��
D�>fD�~�D��
D�  D�@�D��D���D��
D�?
D�~fD��fD��
D�>fD�}�D���D���D�=�D�~D���D��D�>D�~�D���D��
D�?\D�~fD���D��fD�>fD�
D���D���D�?\D�~fD��qD���D�>D�~D��D��D�>fD�~fD���D��
D�>�D�
D��fD��D�?
D�
D��
D���D�?�D��D���D���D�>�D�� D��fD��D�?\D�
D��
D���D�>D�~D���D��
D�>fD�~�D��
D���D�>fD�~�D��fD���D�=�D�~fD�� D� RD�?\D�~fD��
D��fD�>�D�
D���D��fD�@ D��D���D� RD�?\D�~�D��fD��fD�>fD�}D��\D� �D�?\D�
D��fD���D�>�D�~fD���D��D�>D�~�D���D��D�=�D�~D���D��\D�@RD�
D��D���D�>fD�~fD��fD��
D�?\D�
D��fD��D�?
D�\D��
D���D�>�D�~fD��D��fD�>�D��D���D��fD�>�D�
D��\D��\D�>�D�
D���D���D�?�D�~fD���D��D�>�D�\D��RD�  D�>�D�~�D��fD��\D�?
D�~�D��RD��
D�>fD�\D��
D���D�?\D�~fD��D���D�?
D�~fD��D���D�@ D�\D��\D���D�?
D�~�D��\D���D�>fD�~D��D��fD�?\D�� D���D��
D�?�D�~fD¾D��fD�>fD�~�Dÿ
D���D�>fD�~fDĿ
D��
D�>�D�~DžfD��\D�?\D�~�Dƿ
D���D�>�D�\Dǿ\D��fD�>fD�
Dȿ\D��\D�?\D�~�DɾfD���D�>fD�~DʾD���D�>�D�}�D˾D��fD�=�D�~�D̿�D��\D�>�D�~fD;fD��\D�?
D�\Dο\D���D�>fD�~DϿ
D���D�>fD�~Dп
D�  D�?�D��DѾfD��qD�>D��Dҿ�D��D�>�D�\Dӿ
D��\D�?\D�
DԾ�D���D�>fD�~fDտ
D��
D�?�DրRDֿ
D���D�>�D��D׿\D��\D�?
D�
Dؿ\D��D�>fD�~fDپ�D��
D�>D�}Dڽ�D��fD�=qD�~fDۿ�D���D�=qD�~�Dܿ�D�  D�>�D�~Dݾ�D���D�>�D�~D޿\D��
D�=�D�~fD߾�D��qD�>fD�~�D�qD���D�@ D�\DᾸD��fD�?�D�\D�D���D�@ D�~fD��D��fD�?
D�
D�\D���D�?�D�\D��D���D�?\D�
D�\D��\D�?
D�~�D�
D��\D�>�D�~fD�
D��
D�?
D�\D�fD���D�?
D�~fD�
D���D�?
D�
D�\D��fD�>D�}�D쾸D���D�?
D�\D��\D���D�>�D�~fDD��
D�>fD�~D�\D��\D�?
D�\D�D��D�=�D�~fD�\D��\D�?\D��D�fD��D�?
D�
D�D���D�>�D�
D���D���D�>fD�~D��fD��
D�>fD�~fD���D���D�>�D�� D��RD���D�?\D�\D��
D��fD�?\D�
D��\D���D�?�D��D���D���D�?
D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��/A��A��TA���A��A��A���A��pA���A��A��A���A���A��)A��A�یA��5A��pA�ܒA���A��)A��]A�ݘA��;A�ߤA��A��A���A��)A�ںA�خA��EA���A��2A���A�̘A��RA���AǛ�A�{�A�s�A�bA��A�uA�s�Aı'A��A�K�A��nA�7A���A�ܒA�~A�GA��A��=A�L�A�e�A�p�A�1�A�`vA�{JA���A��~A� �A���A���A�o5A�A�3hA���A�uA�uA���A� �A�k�A�~]A�$@A�I�A~�A{�AwR�As�
Aq|�Ap/Ao��AnĜAl[WAi��AfOAbMA^͟A[�
AW�AS��AP�rAN_AH��AERTAC��ABu%A@�8A@QA?\)A=u�A;��A:�A9}VA8��A7o�A5��A3DgA1��A0A�A-e�A,qvA+�EA+oA)�A(l�A'aA&)�A%�_A$�QA$=A$��A$�]A$T�A"�yA!��A%�A�A^�AJ#A$�A�A�MA�eA?�A+�A�oAȴA�A��A�A��AخAMA�A��Av�A��A  A�A��AVmA�A��A+�A�NAz�A�5A�A�AJ�A(�A�A�A��A�$AL0Au�ASA�A��A|Ah�AHA�yA��AF�AA��Au�AC�A�A=�A�A
�4A	��A	%A��AXA��A/A�A�HA��A�zAg�A��A�fA��Ay>AeA�AJ�A�,As�AkQA�A,=Ap;A�A|Ae�AMjA)�A�A RTA F�A I�A -�@�j�@�֡@���@��F@�e�@��N@���@�E9@��]@��+@�A�@�[�@��&@�Xy@�qv@��2@�Ov@�c@���@�Mj@�}V@��3@�J�@��@�<�@�U�@�u�@��@�+@웦@�Xy@�M@�v`@�+@�xl@�h@��@��@��s@�_@�a|@�4@��@�Z�@��@�I�@��+@�x@�Z�@���@�x�@�+�@�o@�j@ऩ@�s@�V@���@�A�@�x@��Z@��@���@ڪe@�_@�@��@ذ!@�%�@�5�@֧@�_@գn@�hs@�@�	@ӭC@�.I@�|�@���@њk@��@о@�N�@�ݘ@�-w@�Ov@�7@��A@�f�@���@�J�@˅�@��@��@��@���@�|�@��@��@�_p@ƞ�@��@Ŵ�@�n/@��U@�)�@��N@�a�@¹$@��@���@���@��@�I�@��z@�o @��v@�Ta@���@�@@��e@�`�@��@��F@�m]@�<6@��@��@���@��O@�,=@��@���@��	@�	l@��\@�]d@�:*@��@��$@�B�@���@��z@�^5@��D@���@���@�=@�ں@��@�`�@��@�rG@�G�@�/�@��@���@�c @� �@���@�2a@�~�@�,=@��0@�y�@�^�@�Dg@��@���@��+@�	�@�e�@��@���@���@���@��Y@�Z@�
�@���@�J#@��X@���@�v�@�Q@��;@�B�@��@��@�u%@��.@��z@���@�1�@���@���@�a|@�-�@��@�]�@��@��O@�A�@��@��>@��N@��V@���@�/@��U@���@��o@�B[@��@��^@��@�6z@���@��p@���@�Q@��@��+@��0@���@�[W@�ߤ@���@�A�@��r@���@�c@�;d@�;@��e@��x@��@��@���@��@��@���@�}V@�H�@���@�s�@�!-@���@�r�@��@��@��X@���@�T�@�<6@�'�@���@���@�GE@�*�@��T@�X@�Y@��@��+@�W�@�!@��F@�t�@�1�@��@���@��r@���@�V@�+k@���@��}@���@���@�m]@���@�L0@�7�@��@�B�@��@��B@��@��+@�>B@��@�  @��9@�/�@��)@��@�Ɇ@��@�|�@�a@�5�@��@��P@��E@��R@���@�\�@�1�@���@���@���@�g�@��@��E@��b@���@���@�y>@�+k@��@���@�IR@�z�@�oi@�D�@��@��@���@��M@�33@��P@�Ĝ@���@�5?@�@��@��C@�RT@���@���@���@��I@�YK@�1'@���@�J#@�4�@��@��@���@�u%@�h�@�W�@�N�@�%�@�r@{J@~�y@~�'@~�b@~�@}�=@}<6@}�@}�@}�@|�@|֡@|��@{�Q@{�@z��@z@�@y?}@xPH@wdZ@w�@vd�@v	@u�@u�S@uJ�@u5�@u@t�@t��@tj@t�@s�g@s�@@sg�@s��@s�@rں@ru@q��@qG�@q+@p�@p��@ptT@o�m@o_p@oE9@odZ@oRT@n�2@nV@m��@m8�@l��@lC-@k�r@k�q@kK�@j�c@j�6@j}V@j?@i�@iu�@h�@h�O@hXy@h	�@g��@g$t@f��@f��@f3�@f�@e��@e��@e \@d��@dS�@cƨ@ct�@c+@b��@b��@b��@bȴ@b��@bJ�@bJ@a��@aS&@a�@`�9@`e�@`2�@_��@_�;@_��@_F�@^�y@^��@^~�@^Ov@^�@]�@]�^@]f�@]�@\�|@\�@\��@\c�@[�@[�@[�@Z�X@Z�b@Z��@Zv�@Y��@X�z@Xj@X~@W�Q@W>�@V�!@VC�@U@U��@U:�@T�f@TɆ@Tu�@T,=@S�m@S��@Ss@S&@R��@R�X@R��@Rs�@R-@R�@Q�@Qԕ@Q�"@QF@Q�@P�I@P-�@O��@O�@N�X@N{�@N_�@M�j@M�^@M��@Mj@M<6@M�@L�O@LQ�@L<�@L�@K��@Ke�@J�@J&�@I}�@H��@HbN@G��@Gƨ@G��@G�@F͟@FQ@E�@E��@E*0@E%@D��@D��@DQ�@C�Q@Cb�@C�@B��@Bxl@B�@A�t@AF@A%F@@��@@�z@@>B@?�@?��@?;d@>�2@>�@>��@>;�@=��@=T�@=�@<֡@<�O@<`�@<7@;�@;�@;��@;_p@;8@:�@:�B@:�L@:� @:xl@:;�@9�@9��@9�n@9rG@97L@8��@8��@8:�@87@8G@7��@7��@7C�@7 i@6ߤ@6�s@6�m@6�b@6�@6��@6YK@60U@6�@6@5�d@5�=@5w2@5^�@5�@4��@4Z@4?�@4  @3�;@3��@3_p@31�@2��@2J�@2�@1�)@1��@1\�@10�@1V@0�@0�$@0m�@0�@/y�@/H�@.�y@.kQ@.;�@-��@-=�@,��@,�9@,��@,�@,y>@,�@+��@+;d@+@*�,@*��@*��@*��@*p;@*Z�@*-@)�z@)��@)a�@)V@)�@(�E@(]d@(D�@(:�@(,=@(�@'�w@'s@'j�@'$t@&�M@&�@&YK@%�.@%�X@%k�@%X@%�@$�|@$��@$S�@#�0@#W?@#"�@"�2@"ȴ@"��@"{�@"($@!�@!f�@!@@ �@ ��@ ��@ S�@��@�6@��@�f@U�@&@�M@��@z@��@^�@IR@:�@#�@	l@��@�.@c�@��@��@{J@�@S@��@^5@#:@ �@ԕ@�@L�@��@��@e�@b@� @��@�P@]�@�@�@H�@:*@6�@	@�@�=@^�@�@��@>B@@�@  @�r@�@��@a@�@�@��@�]@�@�\@�@��@�z@�^@a�@#�@�@��@��@[�@M@9X@@��@6z@�@�@s�@	@�>@��@|@IR@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��/A��A��TA���A��A��A���A��pA���A��A��A���A���A��)A��A�یA��5A��pA�ܒA���A��)A��]A�ݘA��;A�ߤA��A��A���A��)A�ںA�خA��EA���A��2A���A�̘A��RA���AǛ�A�{�A�s�A�bA��A�uA�s�Aı'A��A�K�A��nA�7A���A�ܒA�~A�GA��A��=A�L�A�e�A�p�A�1�A�`vA�{JA���A��~A� �A���A���A�o5A�A�3hA���A�uA�uA���A� �A�k�A�~]A�$@A�I�A~�A{�AwR�As�
Aq|�Ap/Ao��AnĜAl[WAi��AfOAbMA^͟A[�
AW�AS��AP�rAN_AH��AERTAC��ABu%A@�8A@QA?\)A=u�A;��A:�A9}VA8��A7o�A5��A3DgA1��A0A�A-e�A,qvA+�EA+oA)�A(l�A'aA&)�A%�_A$�QA$=A$��A$�]A$T�A"�yA!��A%�A�A^�AJ#A$�A�A�MA�eA?�A+�A�oAȴA�A��A�A��AخAMA�A��Av�A��A  A�A��AVmA�A��A+�A�NAz�A�5A�A�AJ�A(�A�A�A��A�$AL0Au�ASA�A��A|Ah�AHA�yA��AF�AA��Au�AC�A�A=�A�A
�4A	��A	%A��AXA��A/A�A�HA��A�zAg�A��A�fA��Ay>AeA�AJ�A�,As�AkQA�A,=Ap;A�A|Ae�AMjA)�A�A RTA F�A I�A -�@�j�@�֡@���@��F@�e�@��N@���@�E9@��]@��+@�A�@�[�@��&@�Xy@�qv@��2@�Ov@�c@���@�Mj@�}V@��3@�J�@��@�<�@�U�@�u�@��@�+@웦@�Xy@�M@�v`@�+@�xl@�h@��@��@��s@�_@�a|@�4@��@�Z�@��@�I�@��+@�x@�Z�@���@�x�@�+�@�o@�j@ऩ@�s@�V@���@�A�@�x@��Z@��@���@ڪe@�_@�@��@ذ!@�%�@�5�@֧@�_@գn@�hs@�@�	@ӭC@�.I@�|�@���@њk@��@о@�N�@�ݘ@�-w@�Ov@�7@��A@�f�@���@�J�@˅�@��@��@��@���@�|�@��@��@�_p@ƞ�@��@Ŵ�@�n/@��U@�)�@��N@�a�@¹$@��@���@���@��@�I�@��z@�o @��v@�Ta@���@�@@��e@�`�@��@��F@�m]@�<6@��@��@���@��O@�,=@��@���@��	@�	l@��\@�]d@�:*@��@��$@�B�@���@��z@�^5@��D@���@���@�=@�ں@��@�`�@��@�rG@�G�@�/�@��@���@�c @� �@���@�2a@�~�@�,=@��0@�y�@�^�@�Dg@��@���@��+@�	�@�e�@��@���@���@���@��Y@�Z@�
�@���@�J#@��X@���@�v�@�Q@��;@�B�@��@��@�u%@��.@��z@���@�1�@���@���@�a|@�-�@��@�]�@��@��O@�A�@��@��>@��N@��V@���@�/@��U@���@��o@�B[@��@��^@��@�6z@���@��p@���@�Q@��@��+@��0@���@�[W@�ߤ@���@�A�@��r@���@�c@�;d@�;@��e@��x@��@��@���@��@��@���@�}V@�H�@���@�s�@�!-@���@�r�@��@��@��X@���@�T�@�<6@�'�@���@���@�GE@�*�@��T@�X@�Y@��@��+@�W�@�!@��F@�t�@�1�@��@���@��r@���@�V@�+k@���@��}@���@���@�m]@���@�L0@�7�@��@�B�@��@��B@��@��+@�>B@��@�  @��9@�/�@��)@��@�Ɇ@��@�|�@�a@�5�@��@��P@��E@��R@���@�\�@�1�@���@���@���@�g�@��@��E@��b@���@���@�y>@�+k@��@���@�IR@�z�@�oi@�D�@��@��@���@��M@�33@��P@�Ĝ@���@�5?@�@��@��C@�RT@���@���@���@��I@�YK@�1'@���@�J#@�4�@��@��@���@�u%@�h�@�W�@�N�@�%�@�r@{J@~�y@~�'@~�b@~�@}�=@}<6@}�@}�@}�@|�@|֡@|��@{�Q@{�@z��@z@�@y?}@xPH@wdZ@w�@vd�@v	@u�@u�S@uJ�@u5�@u@t�@t��@tj@t�@s�g@s�@@sg�@s��@s�@rں@ru@q��@qG�@q+@p�@p��@ptT@o�m@o_p@oE9@odZ@oRT@n�2@nV@m��@m8�@l��@lC-@k�r@k�q@kK�@j�c@j�6@j}V@j?@i�@iu�@h�@h�O@hXy@h	�@g��@g$t@f��@f��@f3�@f�@e��@e��@e \@d��@dS�@cƨ@ct�@c+@b��@b��@b��@bȴ@b��@bJ�@bJ@a��@aS&@a�@`�9@`e�@`2�@_��@_�;@_��@_F�@^�y@^��@^~�@^Ov@^�@]�@]�^@]f�@]�@\�|@\�@\��@\c�@[�@[�@[�@Z�X@Z�b@Z��@Zv�@Y��@X�z@Xj@X~@W�Q@W>�@V�!@VC�@U@U��@U:�@T�f@TɆ@Tu�@T,=@S�m@S��@Ss@S&@R��@R�X@R��@Rs�@R-@R�@Q�@Qԕ@Q�"@QF@Q�@P�I@P-�@O��@O�@N�X@N{�@N_�@M�j@M�^@M��@Mj@M<6@M�@L�O@LQ�@L<�@L�@K��@Ke�@J�@J&�@I}�@H��@HbN@G��@Gƨ@G��@G�@F͟@FQ@E�@E��@E*0@E%@D��@D��@DQ�@C�Q@Cb�@C�@B��@Bxl@B�@A�t@AF@A%F@@��@@�z@@>B@?�@?��@?;d@>�2@>�@>��@>;�@=��@=T�@=�@<֡@<�O@<`�@<7@;�@;�@;��@;_p@;8@:�@:�B@:�L@:� @:xl@:;�@9�@9��@9�n@9rG@97L@8��@8��@8:�@87@8G@7��@7��@7C�@7 i@6ߤ@6�s@6�m@6�b@6�@6��@6YK@60U@6�@6@5�d@5�=@5w2@5^�@5�@4��@4Z@4?�@4  @3�;@3��@3_p@31�@2��@2J�@2�@1�)@1��@1\�@10�@1V@0�@0�$@0m�@0�@/y�@/H�@.�y@.kQ@.;�@-��@-=�@,��@,�9@,��@,�@,y>@,�@+��@+;d@+@*�,@*��@*��@*��@*p;@*Z�@*-@)�z@)��@)a�@)V@)�@(�E@(]d@(D�@(:�@(,=@(�@'�w@'s@'j�@'$t@&�M@&�@&YK@%�.@%�X@%k�@%X@%�@$�|@$��@$S�@#�0@#W?@#"�@"�2@"ȴ@"��@"{�@"($@!�@!f�@!@@ �@ ��@ ��@ S�@��@�6@��@�f@U�@&@�M@��@z@��@^�@IR@:�@#�@	l@��@�.@c�@��@��@{J@�@S@��@^5@#:@ �@ԕ@�@L�@��@��@e�@b@� @��@�P@]�@�@�@H�@:*@6�@	@�@�=@^�@�@��@>B@@�@  @�r@�@��@a@�@�@��@�]@�@�\@�@��@�z@�^@a�@#�@�@��@��@[�@M@9X@@��@6z@�@�@s�@	@�>@��@|@IR@�1111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BO�BP.BO�BPBP.BP.BPHBP.BPBO�BPBPBP.BP.BPHBP.BP.BP.BPBPBPBPBO�BPBPBPBP.BPBPHBP}BP�BP�BP�BQNBQ�BR�BR�BSuBT�Ba�B|jB��B�mB�;B	(�B	��B	̘B
 �B
��B
��B
��B
�B
��B�B	lB9B�B)�B1�B3hB BB
�[B
��B
چB
��B
��B
�ZB
��B
y�B
tB
lWB
�B
�B	�0B	ݘB	ɆB	�DB	�XB	��B	�B	�/B	�{B	�EB	w�B	m�B	h�B	f�B	c�B	[�B	O\B	>B	.}B	#�B	/B	�B�dB�;B�$B�7BںBٚB�B�"B�;B��B�FB��B	�B		lB	�B	HB	�B	2B	�B	�B	VB	�B	�B	:B	PB	_B	AB	  B	 iB��B�B	aB	�B	#�B	�B	aB	aB	�B	�B	�B	'�B	/�B	9XB	?cB	B�B	HKB	M�B	U�B	d�B	r-B	}�B	�fB	��B	��B	��B	��B	��B	�6B	��B	��B	�BB	��B	��B	��B	�*B	�B	��B	��B	ªB	��B	��B	��B	�AB	�[B	�[B	�B	�B	�HB	�(B	�(B	�(B	�BB	�]B	��B	�'B	ɺB	��B	��B	�hB	уB	уB	ңB	��B	՛B	�B	��B	�aB	��B	�oB	�B	��B	�&B	�B	��B	��B	��B	�uB	�:B	ѝB	бB	�[B	��B	�qB	�B	��B	ބB	خB	��B	�B	�B	�mB	�B	��B	�B	�)B	�2B	�B	�B	�=B	��B	�B	�B	�B	�B	�OB	��B	�B	��B	��B	��B	�/B	� B	� B	�cB	�IB	��B	��B	��B	�B	��B	�0B	��B	�B	�B	�B	�DB	�DB	�_B	��B	�sB	��B	�B	�mB	�RB	�8B	�B	�B	�B	�B	�mB	�mB	��B	�B	��B	�B	�B	��B	�2B	�,B	��B	�B	�&B	��B	�B	� B	�B	�:B	�TB	�TB	�B	��B	�`B	�zB	��B	�mB	��B	�LB	��B	�B	�`B	��B	�B	�B	��B	�B	�B	��B	�DB	�B	�DB	�B	�eB	�0B	�KB	�B	�eB	��B	��B	�eB	�B	�WB	�kB	��B	�QB	�B	��B	��B	�B	�kB	��B	�B	�)B	�CB	�)B	�/B	�;B	�[B	��B	�|B	�aB	��B	�B	�AB	��B	�GB	�3B	�B	�B	�?B	�%B	�tB	��B	�zB	��B	��B	�lB	��B	��B	�$B	��B	��B	��B	��B	��B	�*B	�B	��B	��B	��B	�JB	��B	�B	��B	�"B	�"B	�<B	�"B	�B	�qB	�B	�<B	��B	��B	��B	��B
  B
 OB
 �B
 B
�B
uB
�B
�B
3B
�B
�B
B
9B
mB
tB
�B
�B
	B
	B
	�B
	�B
	lB
	lB
	RB
	RB
	�B

�B
B
�B
~B
�B
B
�B
xB
�B
�B
�B
jB
B
�B
B
�B
B
�B
�B
�B
�B
B
4B
NB
NB
�B
hB
 B
:B
oB
�B
�B
B
�B
,B
{B
�B
�B
{B
MB
�B
�B
�B
�B
�B
�B
sB
?B
sB
?B
YB
YB
�B
�B
�B
�B

B
�B
�B
�B
YB
�B
�B
B
�B
�B
?B
�B
sB
�B

B
B
�B
B
7B
�B
WB
)B
)B
�B
�B
�B
�B
�B
�B
~B
OB
jB
jB
�B
pB
pB
�B
 \B
!�B
"�B
#TB
#nB
#TB
#:B
"hB
"�B
"�B
"4B
!HB
#�B
#�B
$ZB
$tB
$@B
#�B
#nB
# B
#�B
$&B
%�B
&2B
%`B
$@B
$&B
$@B
$&B
$&B
$ZB
$�B
&B
&�B
&�B
'B
'mB
'�B
(�B
)B
)*B
)�B
+B
+kB
+QB
+B
*�B
+QB
+�B
*eB
+B
+�B
,B
,=B
,�B
,�B
-wB
-�B
-]B
,�B
+�B
+kB
*�B
*KB
+B
,"B
,�B
,�B
-�B
-�B
-�B
-CB
-]B
-CB
-�B
.�B
.}B
.�B
/B
/OB
/iB
0B
/�B
0!B
0UB
0UB
1�B
1�B
1vB
1vB
1�B
2B
2-B
2aB
2aB
2GB
2B
1[B
2B
1�B
1vB
1[B
1[B
1vB
1�B
1�B
1�B
1vB
1�B
1�B
1�B
2|B
3�B
4B
4�B
4�B
5�B
6+B
7B
7fB
7�B
7�B
88B
9>B
:DB
:�B
;0B
;dB
;�B
;�B
;�B
<�B
<�B
="B
<�B
<6B
<jB
<�B
<�B
<�B
<�B
<jB
<jB
<jB
<�B
="B
=qB
>]B
>�B
?.B
@�B
A�B
B[B
A�B
BuB
B�B
CB
C-B
C�B
C�B
C�B
C�B
DgB
EB
EB
EB
E9B
ESB
EmB
E�B
FB
F�B
F�B
F�B
F�B
F�B
GzB
G�B
G�B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
IB
IB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
HfB
HfB
HKB
H�B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IRB
I�B
J#B
J#B
J�B
J�B
J�B
K)B
K^B
KxB
K�B
K�B
LB
L0B
LdB
LdB
L�B
L�B
L�B
MB
MB
MPB
MPB
MjB
M�B
M�B
NVB
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
S&B
SB
S&B
S&B
S@B
S@B
S[B
S�B
S�B
T,B
T{B
T{B
TaB
T�B
T�B
U2B
UgB
U�B
U�B
U�B
VB
V9B
V�B
V�B
V�B
W
B
WsB
W�B
W�B
W�B
X+B
X_B
XEB
XyB
X�B
YB
YKB
YeB
Y�B
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[#B
[WB
[�B
[�B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
]dB
]dB
]�B
^B
^B
]�B
]�B
]�B
^B
^OB
^jB
^�B
_;B
_;B
_;B
_pB
_�B
`B
`BB
`vB
`vB
`�B
`�B
`�B
abB
a|B
a�B
a�B
a�B
a�B
a�B
bB
b4B
bNB
bNB
b�B
b�B
b�B
c:B
cnB
cTB
c�B
d&B
dZB
dZB
dZB
d@B
dtB
d�B
e`B
ezB
e�B
e�B
e�B
e�B
e�B
fB
f2B
ffB
f�B
f�B
gB
gRB
gB
gRB
g�B
g�B
g�B
g�B
g�B
h$B
h>B
h>B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
i�B
jB
jB
jKB
jB
kB
k6B
k�B
k�B
k�B
k�B
l"B
lqB
l�B
mB
m]B
m]B
m�B
m�B
m�B
n/B
nIB
n}B
n�B
n�B
n�B
o B
o5B
oiB
p;B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q'B
q�B
q�B
q�B
raB
raB
r�B
r�B
sMB
shB
s�B
s�B
tB
tTB
t�B
t�B
uB
u?B
uZB
utB
utB
u�B
u�B
vzB
v�B
vzB
vzB
v�B
v�B
v�B
w�B
xB
x8B
xlB
xlB
xlB
xlB
xlB
x�B
x�B
y>B
y>B
yXB
yrB
yXB
y�B
z*B
zDB
z^B
zDB
z�B
z�B
{JB
{B
{�B
{�B
{�B
{�B
{�B
|jB
|�B
}"B
}VB
}�B
}�B
~B
~]B
~wB
~�B
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BO�BP.BO�BPBP.BP.BPHBP.BPBO�BPBPBP.BP.BPHBP.BP.BP.BPBPBPBPBO�BPBPBPBP.BPBPHBP}BP�BP�BP�BQNBQ�BR�BR�BS�BT�Ba�B|�B�B��B��B	*B	��B	�(B
#�B
�mB
��B
߾B
��B
�ZB�BBYB�B,=B6�B8RB%�B
��B
�	B
��B
��B
�@B
�$B
��B
|PB
y�B
y�B
B
&B	�'B	�B	�hB	�cB	�/B	��B	�\B	��B	�KB	�DB	z^B	o5B	i�B	hsB	f�B	_pB	S�B	B�B	2�B	'�B	"NB	�B�HB��B�/B��B��B�WB��B�CB��B�hB��B	 OB	zB	
�B	�B	�B	B	YB	;B	"�B	 �B	�B	$B	�B	BB	�B	�B	 �B	UB�HB��B	{B	B	%�B	~B	
B	gB	1B	�B	�B	'�B	0B	9�B	?�B	B�B	H�B	N"B	VB	eB	r�B	~]B	�lB	�SB	�eB	�dB	� B	�6B	��B	�2B	��B	�B	��B	� B	�VB	��B	��B	��B	� B	��B	��B	��B	�'B	B	ªB	ªB	ªB	�B	��B	��B	�BB	�BB	�wB	��B	�.B	��B	�	B	�<B	�4B	��B	�B	�:B	ӏB	ՁB	�mB	�
B	�
B	��B	ӏB	�@B	ѷB	�&B	�[B	�,B	�,B	�{B	ԕB	�,B	��B	�:B	�B	�[B	רB	�=B	�_B	�B	�B	خB	��B	�qB	�yB	�B	�B	�|B	�TB	��B	�fB	�B	��B	��B	�/B	�B	�B	�B	�B	�B	�B	�'B	��B	�[B	�B	��B	��B	�B	��B	��B	�}B	�B	��B	�B	�QB	�B	�eB	�KB	�0B	�DB	��B	�B	��B	�B	��B	�XB	�$B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�8B	��B	�B	��B	�8B	��B	�B	�zB	�,B	�,B	��B	�zB	��B	�nB	�:B	��B	�B	�@B	�&B	�&B	�B	��B	�8B	��B	�mB	��B	�B	�B	��B	�B	�8B	�B	�B	�_B	�yB	�B	�B	�_B	�B	�B	�B	�B	��B	�B	�B	�B	�0B	��B	�WB	��B	�B	�B	��B	�kB	�6B	�6B	�kB	��B	�B	�B	�]B	��B	��B	�B	��B	��B	�B	��B	��B	�-B	��B	��B	�AB	�B	�B	�B	�B	��B	��B	��B	�+B	��B	�LB	�B	��B	��B	�	B	�XB	�DB	�*B	�*B	�B	�DB	��B	�DB	�B	��B	�DB	��B	�6B	�jB	��B	�qB	�VB	��B	�VB	�VB	��B	�<B	��B	�B
  B	��B
  B
 4B
 �B
B
oB
B
�B
�B
�B
gB
�B
�B
mB
�B
�B
�B
KB
�B
	B
	7B
	�B
	�B
	�B
	�B
	�B
	�B

	B

�B
DB
�B
B
JB
dB
�B
�B
JB
�B
6B
�B
<B
�B
<B
�B
\B
�B
 B
 B
4B
NB
NB
�B
�B
�B
�B
oB
TB
�B
�B
&B
@B
�B
aB
�B
�B
�B
�B
�B
�B
9B
B
�B
�B
?B
�B
sB
�B
sB
�B
�B
+B
�B
B
B
�B
�B

B
$B
sB
�B
�B
_B
EB
�B
�B
+B
�B
$B
$B
_B
B
7B
kB
	B
�B
CB
xB
B
/B
�B
�B
B
/B
�B
�B
�B
�B
B
�B
�B
�B
 �B
!�B
"�B
#nB
#�B
#�B
#�B
"�B
"�B
#:B
"�B
!|B
#�B
#�B
$tB
$�B
$ZB
#�B
#�B
#�B
#�B
$&B
&B
&�B
%�B
$ZB
$@B
$ZB
$@B
$@B
$tB
$�B
&LB
&�B
'B
'RB
'�B
'�B
)*B
)DB
)DB
)�B
+B
+�B
+�B
+6B
+B
+�B
,"B
*B
+6B
+�B
,"B
,�B
-)B
-)B
-�B
.B
-�B
-)B
,"B
+�B
+B
*�B
+QB
,=B
,�B
-B
.B
-�B
./B
-�B
-�B
-wB
-�B
.�B
.�B
.�B
/5B
/iB
/�B
0!B
0B
0UB
0oB
0oB
1�B
1�B
1�B
1�B
1�B
2-B
2GB
2�B
2|B
2�B
2GB
1�B
2|B
2GB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
3�B
4B
4�B
5%B
5�B
6+B
72B
7�B
8B
8B
8lB
9rB
:^B
:�B
;JB
;�B
;�B
;�B
;�B
<�B
="B
=VB
="B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=VB
=�B
>�B
?B
?cB
@�B
A�B
B�B
B'B
B�B
B�B
C-B
CGB
C�B
DB
C�B
DB
D�B
E9B
EB
E9B
E9B
ESB
E�B
E�B
F%B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
G�B
HB
G�B
HKB
H�B
H�B
H�B
IB
I7B
IB
H�B
H�B
H�B
IB
IB
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
IB
I�B
I�B
J=B
JXB
J�B
J�B
J�B
K^B
KxB
K�B
K�B
K�B
L0B
LdB
L~B
L~B
L�B
L�B
MB
M6B
M6B
MjB
MjB
M�B
M�B
NB
N�B
N�B
N�B
N�B
PB
P�B
P�B
P�B
Q B
Q B
Q4B
Q�B
Q�B
Q�B
RB
R:B
R�B
R�B
R�B
S&B
S[B
S[B
S[B
S@B
S�B
SuB
S�B
S�B
TB
TaB
T�B
T�B
T{B
T�B
UB
UgB
U�B
U�B
U�B
VB
V9B
VSB
V�B
V�B
V�B
WYB
W�B
W�B
W�B
XB
X_B
X�B
X�B
X�B
YB
Y1B
YeB
YB
Y�B
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[	B
[#B
[#B
[WB
[WB
[qB
[�B
[�B
[�B
[�B
\)B
\CB
\�B
\�B
\�B
\�B
\�B
]B
]B
]~B
]~B
]�B
^B
^B
]�B
]�B
^B
^5B
^jB
^�B
_B
_VB
_VB
_VB
_�B
_�B
`'B
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
bB
bB
b4B
bNB
bhB
b�B
b�B
c B
cB
c�B
c�B
c�B
c�B
dZB
dZB
dtB
dZB
dtB
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
fB
fLB
fLB
f�B
f�B
f�B
gB
gmB
g8B
gmB
g�B
g�B
g�B
g�B
g�B
h>B
hXB
hsB
h�B
h�B
h�B
i_B
iyB
i�B
j0B
jB
j0B
j0B
j�B
j�B
k6B
kkB
k�B
lB
l"B
lB
l=B
l�B
l�B
m)B
m�B
mwB
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
o B
oB
oiB
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
rB
r|B
r|B
r�B
sB
shB
s�B
s�B
s�B
tB
tnB
t�B
t�B
u%B
uZB
utB
u�B
u�B
u�B
vB
vzB
v�B
v�B
v�B
v�B
wB
w2B
w�B
xB
xRB
xlB
xlB
x�B
x�B
x�B
x�B
y	B
y>B
yXB
yrB
y�B
yrB
y�B
zDB
z^B
zxB
z^B
z�B
z�B
{dB
{�B
{�B
{�B
|B
{�B
|B
|�B
|�B
}VB
}qB
}�B
}�B
~(B
~wB
~�B
~�B
.3311111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<V�b<#�
<F?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003300046462020033000464620200330004646202207271135122022072711351220220727113512202207271537342022072715373420220727153734  JA  ARFMdecpA30a                                                                20200319003751  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200319003756  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200319003757  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200319003757  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200319003757  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200319003757  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200319003758                      G�O�G�O�G�O�                JA  ARUP                                                                        20200319005535                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200329154646  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200329154646  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  PSAL_ADJUSTED_QC@
=C�fG�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  TEMP_ADJUSTED_QCC�fC�fG�O�                JM  ARCAJMTM1.0                                                                 20220727023512  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063734  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                