CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-03-08T21:38:08Z creation;2020-03-08T21:38:11Z conversion to V3.1;2022-08-02T05:11:18Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200308213808  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA  A30_8420_040                    2C  D   APEX                            8420                            2.11.2                          846 @��j�| 1   @���.E @.v�����c�C,�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@y��@���A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B���B�  B�ffB�  B�  B�33B���B�33B���C�fC  C  C  C
  C33C��C�fC  C  C�fC  C  C  C  C   C"  C$  C&  C(�C*�C,  C.�C/�fC1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�C3Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@z�H@�{@��RA33A?�
A`(�A�Q�A�=qA�(�A�(�A�Q�A�(�A�  A�A��
B{B
=B�B��B'�
B0
=B8�B?��BH(�BP=qBXffB_�
Bg��Bo�
Bw��B�  B�
=B���B��)B��B���B���B���B�B�B���B�
=B�
=B�B���B�\B�B���B���B�  B�.B�p�B�{B���B�ǮB��B�\)B���B�
=B�33B��HB��B���C�C��C��C�qC
�C(�C�=C�C�RC�C�C��C��C�qC�RC�RC!��C$  C&�C(�C*\C,
=C.�C/�C1�3C3��C6�C8C:C<�C>�C@  CB  CC�qCE��CG�RCI�RCK�qCM�RCO�qCR�CT  CVCX�CZ�C\�C^�C`  Ca�qCd�Cf�Cg�qCj  Cl  Cn  Co��Cq�RCt�Cu��Cw�qCz  C|  C~�C�C��C��)C���C��C�  C��)C��qC��qC�  C�HC�  C�HC��C��)C��qC���C�  C��C��qC��qC�HC��C�  C�  C��C���C�HC�  C���C��)C�  C�  C�  C�  C���C���C��)C��)C��C�fC��qC���C�  C�  C��C��C��C��C��C�HC�  C���C��qC��)C�  C��C���C�  C��C�HC���C���C���C�  C���C�  C��C��qC���C�  C�  C�HC�HC�C�HC��)C�  C�HC�HC��C��C�  C�  C��C��C�  C�  C�HC���C��qC��qC���C�  C��C���C���C���C�  C�C��C���C�HC��C�  C��C��C���C��)C���C���C��)C���C�  C�HC���C���C��)C�  C�HC���C��qC�HC��C���C��)C�HC��D  �D ��D3D��D  D� D �D��D �D� D�D� D  D\D�\D\D  D�HD	  D	~�D
  D
��D�D�HD�\D��D  D��D �D� DHD� D �D�HD��D\D��D~�D �D\D�\D�HDHD\D��D\D�D~D  D~�D�qD�HDHD��D �D��DHD\DHD�3DHD� D�D� D HD � D ��D!~�D!��D"��D#�D#�3D$�D$��D% �D%��D&  D&�HD'HD'\D'�\D(� D)  D)� D)��D*� D*��D+� D,HD,\D,��D-~�D-�\D.~�D.�\D/� D/��D0��D1�D1�HD2  D2\D2��D3~�D4  D4\D5 �D5�HD6 �D6� D7 �D7��D8  D8\D8�\D9��D: �D:��D; �D;\D;�\D<~�D<�D=� D>�D>��D>��D?~�D?��D@��DAHDA~�DA�\DB� DB�\DC~DC�\DD��DE  DE~�DE��DF|�DF�DG��DHHDH��DIHDI\DI�DJ��DJ�\DK~�DK��DL� DMHDM� DN�DN��DO�DO� DO�qDP�HDQ�DQ��DR�DR\DR�\DS\DS�\DT�HDU�DU�HDV  DV��DWHDW��DX  DX~�DX�\DY\DY��DZ~�DZ�\D[�HD\ �D\~�D]  D]�HD]�\D^� D_HD_��D`HD`��Da �Da�HDb�Db��Db��Dc��DdHDd\Dd�De� De��Df|�Df��Dg� Dg��Dh~Dh�\Di��Dj �Dj� Dj�\Dk~�Dl  Dl\Dm  Dm��Dm�\Dn��Do �Do~�Dp  Dp�HDqHDq\Dq��Dr��Ds �Ds~�Dt  Dt��Du �Du��Dv �Dv�HDwDw� Dw�Dx\Dy  Dy\Dz  Dz��D{  D{��D{�\D|~D|��D}� D}�\D~~�D  D\D�\D�@ D�� D�� D��
D�?\D��RD���D�  D�?�D�� D���D� �D�@ D�~�D���D� RD�?\D��RD��RD� �D�AHD���D��RD� RD�@RD���D���D���D�AHD��RD���D� �D�@RD�� D�� D���D�?\D�\D��RD�  D�?
D�\D��
D� RD�AHD���D��\D���D�@RD���D���D�  D�@ D��RD���D�HD�@�D�� D���D���D�@�D�� D��\D���D�@ D��RD��RD���D�?\D���D��HD� �D�@ D�\D���D��\D�@�D���D�ÅD� RD�@ D�\D��\D�  D�@ D��D��\D��
D�?
D�\D���D��\D�@ D��D���D��\D�@RD��RD���D� RD�@RD���D�� D��
D�?\D�\D��
D��\D�?\D�� D��HD� �D�?
D�~�D��
D���D�?\D��D�� D�  D�@ D�\D���D�  D�?�D�\D���D��
D�?�D���D���D��
D�?�D�� D��
D���D�@�D��RD�� D��\D�@RD���D��HD� RD�?�D�
D�� D� �D�@ D���D��HD� RD�?
D�
D��RD���D�?�D���D��RD�  D�?�D�� D��HD� �D�@RD��RD��RD� RD�?�D��D��\D��\D�?\D�
D���D� RD�?�D��D�� D� �D�?�D�\D��RD� �D�@�D���D���D�  D�@ D�� D���D� RD�?\D�
D���D� �D�@ D�\D���D��
D�@ D��RD���D��\D�?
D�\D���D� �D�?
D��D���D��\D�@�D��RD�� D� RD�@RD��D�� D��D�@�D��D���D� RD�?�D�~�D���D�  D�?\D�
D���D� �D�@�D��HD�� D���D�?\D�� D��HD�HD�@RD��D���D� �D�@RD��HD���D� �D�@RD�� D���D�  D�@RD���D��HD�  D�?\D�\D��
D� �D�@RD�� D�� D���D�?\DRD¿\D� RD�A�DÁHD�� D��\D�@RDāHD���D� RD�?\D�
D���D� �D�?
D�
Dƿ
D��
D�?
D�
Dǿ\D�  D�@RDȀ�D���D��D�A�DɀRDɾ�D��\D�?�Dʀ D���D� �D�@�DˁHD���D� RD�?
D�\D�� D� �D�AHD́HD��HD�HD�@RD΀ Dο\D��
D�?�Dπ�D�� D��\D�?
DЀRD���D� �D�@�Dр�D��RD�  D�@RDҁ�D���D�  D�?
DӁHD���D� �D�?�DԀ D���D���D�?
D�\Dտ�D���D�@ D�
Dֿ�D� RD�@ D׀ D׿�D��\D�?�D؀ D��RD� �D�@RD��Dٿ\D���D�@RDڀ�D��RD� RD�?
D�\Dۿ�D� RD�?�D�\Dܿ\D���D�@�D݀�D���D�  D�@ Dހ�D��RD� �D�@�D߁�D���D�  D�?\D��D���D� RD�@RD��D���D� RD�?\D��D⿮D�  D�@�D〤D�� D�  D�@RD��D�� D�  D�@ D� D�� D���D�@ D��D�� D�  D�@RD�RD���D� �D�@�D�
D辸D��
D�?�D�~�D�
D� RD�@RD��D�\D�  D�@ D� D�\D��
D�?�D�RD�� D��fD�>�D��D��\D��
D�@�D��DD���D�@ D��D�fD���D�?\D���D��RD� RD�@�D�RD�\D��
D�>�D�~fD�fD���D�?\D� D���D���D�>fD��D��\D��
D�@�D���D���D��\D�@ D�~�D��\D� RD�?�D�
D��
D��\D�?\D�\D��\D��\D�?�D�� D���D�HD�?�D�~�D���D� �D�A�D�d�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��,A��A��A��)A��A���A��A���A��AA��A��MA��TA��A��MA��A��A��A���A��%A��ZA��+A��fA���A���A���A��2A��A���A��xA��2A��A��A���A��ZA��DA�%A�
�A��A�%A�
	A��A��A��A�A�MA��A��A�A�#�A�&A�(XA�*0A�)�A�A�zxA�>�A���A�4A��A�)�A��9A�+kA�YA��A��A���A�%zA�A�A���A�h
A�+�A��tA���A���A�y	A��xA�FA�?Az��Ax�mAw�6Av�LAuh
As�[Aq5?Am_�Ak��Aj��Ag��Ac�A]�A[TaAY��AT�AO�AM�AJ'RAHffAD�AA��A@ݘA@[�A@{JA>��A:�wA9�YA8�*A7C�A5�#A4�A3d�A0��A0OA/J#A."hA,N<A*��A*U2A)MjA(��A'OvA$u%A#e�A"�A!4A �A��A��A�OAK^A;�A�6AYKA��A��Ah�AA�A(A�HADgA�oAw�A@�AMA�MA�MA��A� AP�AA�AW?AOAh�A��A_�A+kA��A�uAJ�A�QA�uA#�ATaA�2A�hA,�A_pA�oAzA7�A
=A��A�oA7LA��AH�A�Ap;A�A�AA�A
��A
MA	��A	jA	M�A	�As�AخA��A1�AAkQA�ZA�`A��AH�A�PA��A�A�AMAbA�A+�A��A+kA خA �A ��A ��A RT@���@�.I@�4@��L@��>@��)@���@��@�  @��
@���@�
�@���@���@���@��@��@�w2@��B@��Z@��@�/�@���@��$@�<�@�@�8�@���@�j@��A@�:@��@�c @�@��2@��@�,=@��@�]�@�h�@��@�@@�^�@�N�@�k�@�1@琗@�k�@���@��@�B[@�j@�V�@�3�@ᝲ@�A @�M@��@޾@�[�@��@ݤ@@�@܌@�	@�t�@�4@ڕ�@��@��@ّh@�C�@�q@�	l@إz@�� @�m]@��)@�Ta@��A@՟V@�T�@��@ԭ�@ԇ�@�V@��m@�L�@�u%@��@�&@�[�@��@ϯ�@�Dg@��/@��'@η�@Π�@�
�@��@̆Y@��@��9@��d@˥�@�J#@ʩ�@�$�@���@�K�@���@�C�@���@���@ǆ�@�+@ƞ@�$�@���@Ŋ	@�F�@�"�@�~�@���@�zx@�8�@�@��@���@���@�l�@�8@�ی@���@�6@���@��@��@�֡@��@��P@��5@�h
@���@�\)@�/@���@�ں@��h@�u�@�!�@���@�6z@��M@��@�?@�+k@��@��@��@�K�@��`@�PH@��]@��#@��H@��X@�s@��@��9@�8�@�خ@�l�@�,�@��|@���@�]d@�!�@���@��w@�c@�/@�[�@��@�iD@�J�@��@�Q�@�/�@��Z@��@�w2@�#�@�Ɇ@���@�,=@�خ@��-@���@�RT@�%@��@�q�@�4n@��W@��@���@�a@�#�@�@��.@�W�@�4n@�	@���@�7L@�҉@�[�@��@��H@�b�@�q@��M@���@���@�p;@��z@���@�#�@��?@�r�@�$�@�ԕ@���@�>�@���@��@���@�E�@��@�-@�>B@�"h@��
@�F�@���@�;�@�O@�_@���@�S&@���@�|�@�?�@�:*@�-�@�x@��@�s�@��P@�~�@�7@�ԕ@���@�l�@�Mj@�8�@�e,@��@�)�@�M@���@���@�A�@��f@�͟@��o@�� @��V@�~�@�p�@�F@�*0@��@���@�S�@�>B@�O@�ƨ@���@���@�s@�9�@��]@�Ov@�O@��@��d@���@��@���@���@���@��~@�m]@�F�@�+�@��@��	@��@��@�N�@��@��q@�E9@�@@��p@��@���@���@�V�@�7@��d@���@��t@���@��@�s�@�+@���@���@�g8@��@�v`@��@���@��_@�x@���@��@��@���@� i@��@���@��@�'R@�@�	@��m@�~�@��c@�z�@�?�@��@��g@���@�p�@�Q�@�*0@��@��u@�[�@��@���@�`B@�Y@�֡@���@�R�@��@�ԕ@���@�!�@��b@�J�@�?@�@�@�#:@�0@~�R@~�@}Y�@|�`@|�I@|'R@{�*@{o@z��@zL0@y��@y%F@xɆ@xI�@w�}@w{J@w�@v��@vV@v4@u�3@u��@u=�@t��@t9X@s�@s�@s��@s!-@r��@r�,@r�@r@q}�@qQ�@q@@qF@q@p��@p"h@o��@o��@o��@o��@oW?@n�]@m��@l�@ly>@l@k��@k~�@j�2@k�@kS@j�6@i�T@i�'@ihs@i;@h�9@hC-@h  @g�q@gE9@f��@fxl@f�@e�M@d�K@dZ@d�@c�*@cx@b��@b�@b@a��@a	l@`�$@`<�@_�}@_��@_'�@^�R@^4@]��@]��@]2a@\�@\�u@\�@[��@[�@Z�@Z��@ZV@Z�@Y��@YL�@X��@X�@W��@X�@X�@W˒@WH�@V�@V	@U��@U�"@Tz�@S��@S/�@S)_@R��@R�@Q�#@Q�@PɆ@P�z@P�@PA�@O�g@OO@N�@N�s@Nz@N�@M�@Mw2@M�@L�/@L  @K�6@K��@KZ�@J�@J^5@J-@J�@I�H@IG�@I&�@H��@H@G��@GO@F҉@F-@Eԕ@E��@ES&@E;@D��@Dh�@D4n@C��@C�	@C'�@B��@B��@B3�@A�^@ArG@AB�@A<6@@�@@�.@@C-@@%�@?�W@?��@?e�@?,�@>��@>�F@>@=��@=�@=��@=J�@<�@<��@<,=@<�@;�;@;�@;��@;�@;P�@;�@:�]@:�!@:n�@:	@9�H@9�@9%@8��@8[�@87�@8�@7�A@7ƨ@7��@7RT@7/�@7"�@6ߤ@6�@6Ta@6-@6O@6�@6�@5��@5��@50�@4�z@4g8@4�@3��@3��@3��@3W?@2��@2��@2\�@2($@2	@1�@1�n@1#�@0�@04n@/�@/y�@/�@.��@.J�@.	@-:�@,�	@,��@,��@,�z@,/�@,  @+��@+�f@+H�@+(@*�X@*�F@*$�@)ԕ@)��@)s�@(��@(oi@(%�@'��@'P�@'J#@'A�@&��@&��@&��@&Ov@&{@%��@%��@%��@%�N@%a�@$�	@$�@$�E@$�O@$��@$r�@$h�@$7�@#ݘ@#�K@#ƨ@#��@#��@#iD@"�\@"l�@"#:@!�@!��@!\�@ �@ ��@ �@ :�@ @��@@O@��@��@�@h
@�@�@T�@q@��@�@��@�_@_@�@��@��@��@|�@|�@4�@�c@� @?@&�@�@��@|@k�@�@�)@H@	�@��@x@n/@_p@>�@��@�h@��@l�@ �@��@��@�@�=@��@\�@N<@IR@*0@�@;@�@��@�o@~(@Z@!@�r@˒@��@l�@1�@ i@��@��@��@��@^5@)�@�Z@�@�n@�=@�"@|@5�@�|@��@��@z�@e�@2�@�@�m@�K@��@j�@Mj@�@�2@�!@u%@L0@)�@#:@�@��@�9@�X@^�@8�@�@��@�@�j@��@~(1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��,A��A��A��)A��A���A��A���A��AA��A��MA��TA��A��MA��A��A��A���A��%A��ZA��+A��fA���A���A���A��2A��A���A��xA��2A��A��A���A��ZA��DA�%A�
�A��A�%A�
	A��A��A��A�A�MA��A��A�A�#�A�&A�(XA�*0A�)�A�A�zxA�>�A���A�4A��A�)�A��9A�+kA�YA��A��A���A�%zA�A�A���A�h
A�+�A��tA���A���A�y	A��xA�FA�?Az��Ax�mAw�6Av�LAuh
As�[Aq5?Am_�Ak��Aj��Ag��Ac�A]�A[TaAY��AT�AO�AM�AJ'RAHffAD�AA��A@ݘA@[�A@{JA>��A:�wA9�YA8�*A7C�A5�#A4�A3d�A0��A0OA/J#A."hA,N<A*��A*U2A)MjA(��A'OvA$u%A#e�A"�A!4A �A��A��A�OAK^A;�A�6AYKA��A��Ah�AA�A(A�HADgA�oAw�A@�AMA�MA�MA��A� AP�AA�AW?AOAh�A��A_�A+kA��A�uAJ�A�QA�uA#�ATaA�2A�hA,�A_pA�oAzA7�A
=A��A�oA7LA��AH�A�Ap;A�A�AA�A
��A
MA	��A	jA	M�A	�As�AخA��A1�AAkQA�ZA�`A��AH�A�PA��A�A�AMAbA�A+�A��A+kA خA �A ��A ��A RT@���@�.I@�4@��L@��>@��)@���@��@�  @��
@���@�
�@���@���@���@��@��@�w2@��B@��Z@��@�/�@���@��$@�<�@�@�8�@���@�j@��A@�:@��@�c @�@��2@��@�,=@��@�]�@�h�@��@�@@�^�@�N�@�k�@�1@琗@�k�@���@��@�B[@�j@�V�@�3�@ᝲ@�A @�M@��@޾@�[�@��@ݤ@@�@܌@�	@�t�@�4@ڕ�@��@��@ّh@�C�@�q@�	l@إz@�� @�m]@��)@�Ta@��A@՟V@�T�@��@ԭ�@ԇ�@�V@��m@�L�@�u%@��@�&@�[�@��@ϯ�@�Dg@��/@��'@η�@Π�@�
�@��@̆Y@��@��9@��d@˥�@�J#@ʩ�@�$�@���@�K�@���@�C�@���@���@ǆ�@�+@ƞ@�$�@���@Ŋ	@�F�@�"�@�~�@���@�zx@�8�@�@��@���@���@�l�@�8@�ی@���@�6@���@��@��@�֡@��@��P@��5@�h
@���@�\)@�/@���@�ں@��h@�u�@�!�@���@�6z@��M@��@�?@�+k@��@��@��@�K�@��`@�PH@��]@��#@��H@��X@�s@��@��9@�8�@�خ@�l�@�,�@��|@���@�]d@�!�@���@��w@�c@�/@�[�@��@�iD@�J�@��@�Q�@�/�@��Z@��@�w2@�#�@�Ɇ@���@�,=@�خ@��-@���@�RT@�%@��@�q�@�4n@��W@��@���@�a@�#�@�@��.@�W�@�4n@�	@���@�7L@�҉@�[�@��@��H@�b�@�q@��M@���@���@�p;@��z@���@�#�@��?@�r�@�$�@�ԕ@���@�>�@���@��@���@�E�@��@�-@�>B@�"h@��
@�F�@���@�;�@�O@�_@���@�S&@���@�|�@�?�@�:*@�-�@�x@��@�s�@��P@�~�@�7@�ԕ@���@�l�@�Mj@�8�@�e,@��@�)�@�M@���@���@�A�@��f@�͟@��o@�� @��V@�~�@�p�@�F@�*0@��@���@�S�@�>B@�O@�ƨ@���@���@�s@�9�@��]@�Ov@�O@��@��d@���@��@���@���@���@��~@�m]@�F�@�+�@��@��	@��@��@�N�@��@��q@�E9@�@@��p@��@���@���@�V�@�7@��d@���@��t@���@��@�s�@�+@���@���@�g8@��@�v`@��@���@��_@�x@���@��@��@���@� i@��@���@��@�'R@�@�	@��m@�~�@��c@�z�@�?�@��@��g@���@�p�@�Q�@�*0@��@��u@�[�@��@���@�`B@�Y@�֡@���@�R�@��@�ԕ@���@�!�@��b@�J�@�?@�@�@�#:@�0@~�R@~�@}Y�@|�`@|�I@|'R@{�*@{o@z��@zL0@y��@y%F@xɆ@xI�@w�}@w{J@w�@v��@vV@v4@u�3@u��@u=�@t��@t9X@s�@s�@s��@s!-@r��@r�,@r�@r@q}�@qQ�@q@@qF@q@p��@p"h@o��@o��@o��@o��@oW?@n�]@m��@l�@ly>@l@k��@k~�@j�2@k�@kS@j�6@i�T@i�'@ihs@i;@h�9@hC-@h  @g�q@gE9@f��@fxl@f�@e�M@d�K@dZ@d�@c�*@cx@b��@b�@b@a��@a	l@`�$@`<�@_�}@_��@_'�@^�R@^4@]��@]��@]2a@\�@\�u@\�@[��@[�@Z�@Z��@ZV@Z�@Y��@YL�@X��@X�@W��@X�@X�@W˒@WH�@V�@V	@U��@U�"@Tz�@S��@S/�@S)_@R��@R�@Q�#@Q�@PɆ@P�z@P�@PA�@O�g@OO@N�@N�s@Nz@N�@M�@Mw2@M�@L�/@L  @K�6@K��@KZ�@J�@J^5@J-@J�@I�H@IG�@I&�@H��@H@G��@GO@F҉@F-@Eԕ@E��@ES&@E;@D��@Dh�@D4n@C��@C�	@C'�@B��@B��@B3�@A�^@ArG@AB�@A<6@@�@@�.@@C-@@%�@?�W@?��@?e�@?,�@>��@>�F@>@=��@=�@=��@=J�@<�@<��@<,=@<�@;�;@;�@;��@;�@;P�@;�@:�]@:�!@:n�@:	@9�H@9�@9%@8��@8[�@87�@8�@7�A@7ƨ@7��@7RT@7/�@7"�@6ߤ@6�@6Ta@6-@6O@6�@6�@5��@5��@50�@4�z@4g8@4�@3��@3��@3��@3W?@2��@2��@2\�@2($@2	@1�@1�n@1#�@0�@04n@/�@/y�@/�@.��@.J�@.	@-:�@,�	@,��@,��@,�z@,/�@,  @+��@+�f@+H�@+(@*�X@*�F@*$�@)ԕ@)��@)s�@(��@(oi@(%�@'��@'P�@'J#@'A�@&��@&��@&��@&Ov@&{@%��@%��@%��@%�N@%a�@$�	@$�@$�E@$�O@$��@$r�@$h�@$7�@#ݘ@#�K@#ƨ@#��@#��@#iD@"�\@"l�@"#:@!�@!��@!\�@ �@ ��@ �@ :�@ @��@@O@��@��@�@h
@�@�@T�@q@��@�@��@�_@_@�@��@��@��@|�@|�@4�@�c@� @?@&�@�@��@|@k�@�@�)@H@	�@��@x@n/@_p@>�@��@�h@��@l�@ �@��@��@�@�=@��@\�@N<@IR@*0@�@;@�@��@�o@~(@Z@!@�r@˒@��@l�@1�@ i@��@��@��@��@^5@)�@�Z@�@�n@�=@�"@|@5�@�|@��@��@z�@e�@2�@�@�m@�K@��@j�@Mj@�@�2@�!@u%@L0@)�@#:@�@��@�9@�X@^�@8�@�@��@�@�j@��@~(1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Ba�BabB`�B`�BaBa-BaBaB`�B`�BaB`�Ba-BaBaB`�B`�BaBa-Ba-BaBaHB`�BaHBa-BaBaBa-BabBaHBaHBaBa-BaHBa�Bc�Bd�BcnBcTBd&BfBf�Bf�Bf�Bf�BhXBjKBj0BlWBpBvzB�B��B��B	F�B	��B
JB
+�B
�:B
��B
�_B
��B
��B
&�B
_B
�B
AB	��B
oB	�B	��B
�B
uB	�B	�dB	רB	��B	�rB	�bB	��B	��B	�B	�=B	��B	r�B	a�B	U�B	P�B	C{B	5�B	"hB	$B	�B�cB��B߾B��B�B�B՛B�VB�NB��B	 OB	tB	B	aB	�B		lB	�B	B	 �B	�B	�B	
B	.B	SB	,�B	9�B	:xB	:xB	=<B	<B	9$B	3hB	4TB	7�B	9>B	>�B	?B	=<B	E9B	mCB	|�B	.B	��B	��B	��B	�6B	��B	�lB	��B	�"B	�B	��B	�PB	��B	��B	�6B	�]B	��B	�uB	��B	��B	��B	�YB	�B	�%B	ňB	�B	�B	�9B	�gB	��B	�UB	B	�B	�uB	��B	�[B	�{B	�GB	��B	ĜB	żB	�KB	�7B	��B	ǔB	ǔB	�1B	��B	οB	�"B	�pB	ϑB	ϫB	�.B	�pB	�B	�dB	�xB	̘B	��B	�bB	��B	�4B	ңB	өB	��B	�2B	�$B	�?B	��B	�$B	׍B	ּB	�YB	��B	�B	��B	��B	�CB	چB	ٚB	��B	�/B	��B	�'B	�|B	�HB	�pB	�HB	�|B	�pB	�B	�B	�mB	�B	�qB	�qB	�KB	��B	�8B	�B	��B	�0B	��B	�B	�B	��B	�B	�B	�eB	�B	�QB	�B	�kB	�B	�KB	�_B	�KB	��B	�kB	�B	�eB	�B	�B	�_B	�XB	�B	��B	�B	�B	�B	�ZB	��B	�B	�*B	�B	�B	�B	��B	�`B	��B	�B	�B	��B	�ZB	��B	�B	��B	��B	�B	�B	��B	�B	�B	�8B	��B	�sB	�XB	�$B	�
B	�>B	�B	�B	�eB	�B	�B	�B	�>B	��B	��B	�fB	�2B	�fB	��B	�8B	�B	�B	��B	��B	�>B	��B	�B	�_B	�B	��B	�B	�XB	�*B	��B	�B	�QB	�qB	�B	�wB	�)B	��B	�cB	�OB	�B	�!B	�oB	��B	�B	��B	�AB	�vB	�|B	�3B	�MB	�hB	�B	�B	�nB	��B	�B	��B	��B	��B	��B	�zB	�zB	��B	��B	��B	�$B	�XB	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	�0B	�dB	�B	��B	��B	�"B	�B	�B	��B	��B	��B	��B	��B	�VB	�wB	�.B
 B
 B
 iB
 �B
oB
�B
�B
B
GB
�B
9B
�B
gB
B
3B
B
{B
�B
�B
�B
mB
�B
B
B
�B
�B
�B
�B
	B

XB

�B
�B
dB
�B
"B
<B
B
�B
�B
pB
�B
�B
B
BB
�B
�B
VB
�B
B
hB
�B
uB
@B
�B
�B
�B
�B
@B
�B
@B
,B
�B
B
�B
�B
9B
B
�B
�B
_B
_B
+B
�B
�B
sB
?B
YB
�B
yB
yB
B
�B
�B
�B
B
�B
�B
B
�B
�B
QB
�B
1B
�B
kB
�B
WB
B
�B
WB
�B
	B
�B
QB
QB
�B
�B
�B
�B
IB
B
�B
~B
�B
~B
B
B
jB
jB
�B
�B
�B
B
�B
!B
�B
 'B
 \B
 BB
 \B
 vB
 �B
 �B
!bB
!�B
!�B
"4B
"4B
"�B
#TB
$ZB
%B
%�B
%�B
&2B
&LB
&LB
&�B
'B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
(>B
(�B
(�B
)DB
(>B
'B
&LB
%�B
%�B
&2B
%�B
%�B
%�B
%�B
&�B
&LB
&�B
&�B
'mB
'�B
'�B
(
B
)*B
)�B
*eB
*eB
*KB
*0B
*�B
,B
,WB
,�B
-)B
,�B
-)B
/�B
/�B
0!B
0oB
0�B
1B
1�B
1�B
1�B
2GB
2�B
3�B
3�B
3�B
3�B
4B
49B
5B
5ZB
5�B
6B
6+B
6�B
6�B
7fB
7�B
7�B
7�B
88B
8RB
8�B
9$B
9rB
9�B
9�B
:*B
:DB
:�B
:�B
:�B
;B
;0B
:�B
:�B
;0B
;JB
;�B
;�B
;�B
<�B
;�B
;dB
;0B
<6B
<�B
<6B
;dB
;B
<�B
=�B
=qB
>(B
>B
=<B
<PB
<B
;�B
;�B
;�B
<jB
>BB
?cB
?�B
@B
@B
@�B
@�B
@iB
@OB
@4B
@B
@ B
@OB
@�B
A B
AUB
A�B
B'B
BAB
BuB
B�B
B�B
BAB
BuB
BB
A�B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
BuB
C-B
DB
E�B
F%B
F?B
E�B
E�B
E�B
EmB
D�B
E�B
F%B
F�B
HB
IB
H�B
H�B
H�B
HKB
I�B
J�B
I�B
H�B
IB
I7B
J=B
J�B
J�B
KB
J�B
J�B
KB
J�B
K^B
L�B
M�B
M�B
NVB
N"B
NB
N"B
NB
M�B
N�B
N�B
OB
O�B
P�B
P�B
P�B
P�B
P�B
Q4B
QNB
Q�B
R B
RTB
R�B
S@B
S�B
S�B
T,B
TB
TB
TB
T,B
TB
T�B
U�B
VSB
VB
VSB
V�B
W?B
W�B
W�B
W�B
W�B
XB
X+B
X+B
X�B
X�B
X�B
X�B
YB
YeB
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
Z7B
Z�B
Z�B
[=B
[qB
[qB
[qB
[�B
[�B
\)B
\)B
\CB
\xB
\xB
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
]�B
^B
^5B
^B
^5B
^5B
^OB
^jB
^OB
^jB
^jB
^�B
^�B
^�B
_pB
_�B
_�B
_�B
_�B
_�B
`B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
aB
a�B
a�B
a�B
bB
b4B
b�B
b�B
cB
c�B
c�B
dB
dB
dB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
ezB
e�B
e�B
e�B
fLB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
jKB
jeB
jeB
jeB
jeB
k�B
kkB
k�B
l"B
l"B
lWB
l�B
mB
mB
m]B
mwB
m�B
n/B
ncB
n�B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
pB
p!B
p;B
poB
p�B
q'B
qAB
q[B
q[B
qAB
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
r�B
shB
s�B
s�B
t9B
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
v�B
wB
wLB
w�B
w�B
w�B
w�B
xB
xB
x8B
x�B
x�B
x�B
y$B
y>B
yXB
yXB
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{�B
{�B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
}B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
B
HB
.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Ba�BaHB`�B`�BaBa-BaBaB`�B`�BaB`�Ba-BaBaB`�B`�BaBa-Ba-BaBaHB`�BaHBa-BaBaBa-BabBaHBaHBaBa-BaHBa�Bc�Bd�BcnBcTBd&BfBf�Bf�Bf�Bf�BhXBjKBj0BlWBpBv�B�iB��B�[B	K^B	��B
�B
;B
�B
�B
��B
��B
��B
*�B
 �B
hB
_B
?B
�B
�B
1B
�B
�B	�!B	�&B	�B	�7B	��B	��B	�+B	�{B	��B	��B	�B	v�B	dZB	XB	T�B	H�B	;�B	%�B	�B	�B	B�B�TB�B��B�B��B�B�TB��B	MB	�B	�B	SB	�B	)B	�B	�B	!�B	 B	�B	KB	�B	sB	-�B	;B	<�B	=�B	>�B	=�B	:^B	49B	6+B	9�B	9�B	?�B	@B	=<B	DB	l�B	|�B	cB	�B	�B	�QB	��B	�3B	�	B	��B	��B	��B	�HB	�B	�JB	�JB	��B	��B	�oB	��B	ǮB	ǔB	�gB	ƨB	ňB	ƨB	�B	ŢB	ňB	��B	�mB	��B	��B	�aB	�B	�B	�[B	��B	��B	ðB	�MB	�B	ƎB	ȴB	��B	�lB	�B	��B	��B	�B	�\B	ΥB	��B	��B	�B	��B	�(B	̈́B	��B	�B	�B	�pB	ЗB	�bB	ѷB	�B	�B	�MB	�B	��B	רB	�EB	�B	�EB	�YB	��B	��B	�KB	�)B	�~B	��B	ںB	��B	�)B	ݘB	��B	�vB	��B	��B	߾B	��B	��B	߾B	�B	�B	�mB	�*B	��B	��B	�B	��B	�B	��B	�*B	�B	�)B	��B	�kB	�0B	�B	�eB	��B	�B	��B	�B	�B	��B	�B	��B	��B	�"B	�B	�B	�B	�KB	�_B	�B	�B	�$B	�B	�B	�B	��B	�B	�8B	��B	��B	�B	�:B	�TB	�B	�B	�RB	�fB	�B	�,B	�B	�@B	��B	��B	�B	��B	��B	�2B	�B	�
B	�B	�XB	��B	�B	�XB	�>B	�B	�
B	��B	�B	��B	�>B	�B	�B	�RB	�RB	�B	�fB	�B	�B	�RB	�8B	�8B	�RB	�mB	�B	�DB	��B	�yB	�DB	�B	��B	��B	�_B	�eB	�B	�B	�B	��B	��B	�wB	�B	�B	�B	��B	�UB	�B	�B	�B	�'B	�B	��B	��B	�MB	�B	�B	��B	�TB	��B	��B	�ZB	�B	�?B	�ZB	�FB	��B	�B	�fB	�B	��B	�>B	��B	��B	��B	��B	�*B	��B	�dB	��B	��B	��B	��B	�JB	��B	�PB	�<B	�B	��B	�<B	�"B	��B	��B	��B	��B	�B	��B	��B	�}B
 OB
 OB
 �B
 B
�B
�B
B
GB
�B
MB
�B
B
�B
gB
�B
3B
�B
�B
3B
B
�B
�B
tB
EB
�B
B
B
KB
	lB

rB
)B
B
�B
�B
<B
pB
<B
B
"B
�B
�B
B
bB
�B
B
�B
�B
B
NB
�B
B
�B
�B
[B
B
FB
FB
�B
&B
�B
aB
�B
MB
�B
�B
mB
9B
�B
�B
�B
�B
�B
EB
B
�B
?B
�B
+B
�B
�B
EB
�B
�B
�B
7B
7B
QB
�B
KB
KB
�B
�B
KB
�B
kB
B
�B
B
�B
�B
�B
WB
�B
�B
�B
�B
	B
�B
�B
dB
IB
IB
�B
B
�B
OB
5B
jB
�B
�B
B
!B
;B
B
!B
�B
 'B
 \B
 \B
 \B
 �B
 �B
!B
!|B
!�B
"B
"NB
"hB
#B
#�B
$�B
%`B
&2B
&B
&LB
&fB
&fB
&�B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(sB
(�B
)B
)�B
(�B
'RB
&fB
%�B
&B
&LB
%�B
&B
%�B
&2B
&�B
&�B
&�B
&�B
'�B
'�B
(
B
(XB
)�B
*KB
*�B
*�B
*eB
*eB
*�B
,"B
,�B
-)B
-wB
,�B
-wB
/�B
/�B
0;B
0�B
0�B
1AB
1�B
1�B
2GB
2�B
2�B
3�B
3�B
4B
4B
4TB
4�B
5?B
5�B
5�B
6+B
6`B
6�B
72B
7�B
7�B
7�B
8B
8lB
8�B
8�B
9>B
9�B
9�B
9�B
:DB
:^B
:�B
:�B
:�B
;JB
;JB
;B
:�B
;JB
;dB
;�B
;�B
<B
<�B
;�B
;B
;B
<PB
<�B
<jB
;�B
;B
<�B
=�B
=�B
>]B
>wB
=�B
<�B
<6B
;�B
;�B
<B
<jB
>]B
?�B
?�B
@4B
@B
@�B
@�B
@�B
@iB
@OB
@4B
@OB
@iB
A;B
AUB
A�B
A�B
BAB
BuB
B�B
B�B
B�B
B[B
B�B
BAB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
BAB
B�B
CaB
DMB
E�B
F?B
FYB
F%B
FB
E�B
E�B
EB
E�B
F%B
F�B
HB
IB
H�B
IB
H�B
H�B
J#B
KB
J#B
IB
I7B
IlB
JrB
J�B
J�B
K)B
J�B
KB
K)B
K)B
K�B
L�B
NB
M�B
NpB
N<B
N<B
N<B
N<B
NB
N�B
N�B
OBB
PB
P�B
P�B
P�B
P�B
P�B
QNB
Q�B
Q�B
RTB
R�B
R�B
SuB
S�B
S�B
TFB
T,B
T,B
T,B
TFB
TFB
T�B
U�B
VmB
V9B
VmB
W
B
WYB
W�B
W�B
W�B
W�B
X+B
XEB
XEB
X�B
X�B
X�B
X�B
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
ZQB
ZkB
Z�B
Z�B
[WB
[qB
[�B
[�B
[�B
[�B
\CB
\]B
\xB
\�B
\�B
\�B
]B
]dB
]�B
]�B
]�B
]�B
]�B
^B
^5B
^OB
^5B
^OB
^OB
^jB
^jB
^OB
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
`'B
`B
`'B
`BB
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
bNB
bhB
b�B
b�B
cTB
c�B
c�B
dB
d&B
d@B
eB
d�B
d�B
d�B
d�B
d�B
eB
eFB
e�B
e�B
e�B
e�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h$B
h$B
hsB
h�B
h�B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
i�B
jB
i�B
j0B
jeB
jKB
jB
jB
jB
j�B
k�B
kkB
k�B
l=B
l=B
l�B
mB
m)B
m)B
mwB
m�B
m�B
nIB
n}B
n�B
n�B
o B
oOB
o�B
o�B
o�B
o�B
p!B
p;B
pUB
p�B
p�B
qAB
qAB
qAB
q[B
q[B
q�B
q�B
rGB
raB
r�B
r�B
r�B
sB
sB
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
w2B
wLB
w�B
w�B
w�B
w�B
xB
xB
xRB
x�B
x�B
y	B
y$B
yrB
yrB
yrB
y�B
y�B
z*B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{�B
{�B
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
}B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~�B
~�B
~�B
~�B
B
.B
HB
.3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<|PH<#�
<#�
<#�
<#�
<o4�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003200111332020032001113320200320011133202207271135052022072711350520220727113505202207271537262022072715372620220727153726  JA  ARFMdecpA30a                                                                20200308213801  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200308213808  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200308213809  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200308213810  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200308213811  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200308213811  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200308213811                      G�O�G�O�G�O�                JA  ARUP                                                                        20200308215448                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200310000000  CF  PSAL_ADJUSTED_QC@��@z�HG�O�                JM  ARCAJMQC2.0                                                                 20200319161133  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200319161133  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023505  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063726  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                