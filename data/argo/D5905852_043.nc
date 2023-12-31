CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-08T03:38:28Z creation;2020-04-08T03:38:30Z conversion to V3.1;2022-08-02T05:11:11Z update;     
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
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200408033828  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  A30_8420_043                    2C  D   APEX                            8420                            2.11.2                          846 @�<:Ӏ1   @�<��O�@03�����cD���#�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B왚B�ffB���B�  B�  B���C�fC  C�C  C	�fC�fC  C  C�CL�C�3C  C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4ffC5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CXffCY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @2�\@}p�@�  A Q�A�A?�A`  A�{A��A�{A�Q�A�A��A�ffA�{A��
B  B  B�B {B'�HB/�HB7��B@
=BG��BO�HBW�B`
=Bh
=Bp  Bx{B�\B�
=B���B���B�  B�B�B�B�  B���B�B�
=B���B��B�  B�\B�  B���B�  B���B�B�\B�B���B�B�{B�=qB왚B�ffB�qB���B��B���C�C��C�C�C	�fC�C�qC�C�CG�C�fC�qC�C�C��C�qC!�qC$�C&�C(�C)�RC+�qC.�C0�C1��C4aHC5�3C7�3C9�RC<  C>�C?�qCB  CDCE�RCG��CI��CK��CM�qCO��CQ�RCT�CV
=CX^�CY�)C[�C]��C_��Ca��Cc�qCe�qCh�Cj
=Cl�Cn  Co�3Cq��Cs��Cu�qCx  Cz�C|  C}��C�3C��qC��qC�  C��C���C���C�C�HC�HC�HC��C�HC�  C��qC���C���C���C��C�HC���C��qC�  C�HC�HC�  C���C�  C�HC�  C���C��)C���C��qC��qC��qC�  C�  C��qC���C��qC��)C��)C���C�C�  C��qC��qC���C�HC�HC�  C�  C��qC�  C��C�  C�HC��C�HC��qC���C��)C��)C�  C��C�  C�  C�HC�  C�HC��C��C��C�HC��qC�  C��C���C��qC�HC��C�fC�HC��qC��C��C�HC��C��C��)C���C���C�  C��C�HC�HC�HC��C���C���C�  C��)C���C�  C��C��qC�  C�C��C��C�  C�  C�HC��C�HC�HC��C���C�HC�  C���C�  C�HC�  C��qC�  C���C���D � D �D� D��D~D�D\D �D�HDHD� D �D� D�qD~�D��D~�D	  D	� D
  D
\D
��D��D�\D|�D�\D� D �D��D�D� D  D�HD �D��D�D��D �D��D�\D~�D�\D�HD  D~�DHD��D  D~�DHD� D�\D~D��D� D�\D\D��D~�D �D��D{D�HD   D � D �\D!��D!��D"\D# �D#� D#��D$~�D$��D%~D&  D&�HD' �D'�HD'�\D(~D(�\D)��D)�\D*~D*�\D+��D, �D,�HD-�D-��D-�\D.}qD.�\D/�HD0 �D0� D0��D1� D2HD2\D2��D3~�D3�\D4� D4�D5� D6 �D6�HD7�D7�HD8 �D8�HD9  D9� D:  D:}qD:�\D;��D<  D<~D= �D=�HD=��D>~�D>�\D?� D@ �D@\DAHDA��DA��DB� DC�DC��DDHDD\DD�\DE\DF  DF�HDG �DG\DH  DH~�DH�DI� DJHDJ��DJ�\DK�HDL �DL� DM�DM~�DM��DN}qDN�DO~�DO��DP� DQ �DQ~DQ��DR\DS  DS� DS�\DT~DUHDU�HDU�\DV~�DV�\DW\DW�\DX~�DY  DY�HDZ  DZ� D[ �D[��D\ �D\��D]HD]��D]��D^~�D^�\D_��D`HD`�HDaHDa� Da��Db� DcHDc�HDdHDd��De  De\De�\Df��Dg  Dg\Dh �Dh�HDi  Di� DjHDj� Dk  Dk�HDl  Dl~�Dl��Dm\Dn �Dn\Dn�\Do� Do��Dp}qDq �Dq�HDq�\Dr� Ds  Ds�3Dt�Dt� Du �Du~�Du�Dv\Dv�\Dw~Dw��Dx��Dy �Dy��DzHDz�HD{  D{~D{��D|� D}HD}��D~HD~��D~��D�HD� �D�@ D�� D���D� RD�?�D�
D��RD� �D�@�D���D�� D� RD�@RD��D�� D� �D�@RD���D���D��\D�>�D�� D��RD� �D�AHD���D���D� RD�@ D�\D���D� RD�@ D�� D�� D�  D�@�D��RD���D��\D�?\D�� D��\D���D�?�D���D���D� �D�@�D��RD���D� �D�@ D�� D���D���D�@RD��HD���D� RD�@ D��RD��RD� �D�@�D�� D��
D�  D�AHD���D���D� �D�@RD��RD��RD� �D�@RD���D��RD� RD�@ D��RD��RD� �D�?�D�~�D��
D��\D�@ D��RD���D� �D�A�D���D���D��\D�?�D���D���D� �D�?�D�\D��\D��\D�?�D���D��RD� RD�@RD�\D�� D�  D�?�D��D��RD� �D�?�D��D���D� RD�@ D�\D���D� �D�A�D�� D��\D� RD�@ D��RD���D���D�>�D�\D���D� �D�@RD�~�D��
D�  D�@�D��RD���D��D�?\D�\D���D� �D�@ D�\D�� D� �D�@�D��D��RD���D�?\D���D���D��fD�?
D�� D���D��\D�@ D���D��RD���D�@�D���D��RD� �D�AHD���D�� D� �D�@RD��RD��RD� RD�@ D��D��HD� RD�?\D��D���D��
D�?�D���D���D� RD�@ D���D��RD�  D�@�D��RD���D�  D�?�D��D���D��\D�@RD��D��\D��\D�?\D�\D���D���D�@�D���D���D� RD�@RD���D��\D��\D�?
D��D���D� �D�?�D��D���D�HD�@�D��D���D� �D�@RD�� D�� D� �D�@ D��RD���D�  D�@ D�� D���D� �D�@�D��RD�� D� RD�@RD��D��RD�  D�>fD�
D��RD�  D�@ D��RD���D��
D�?
D��D���D���D�?
D�~fD¿�D� �D�@�DÁ�D���D���D�@�DāHD�� D���D�?�DŀRDſ�D��fD�?
Dƀ Dƿ
D��D�?\D�\Dǿ\D���D�?
DȀ�D���D� RD�@ D�\Dɿ\D�  D�@�DʀRDʿ\D��\D�?
D�~�D˿\D�  D�?�D��D�� D� RD�?�D�\DͿ\D��\D�@RD΀ Dο\D���D�@ D��DϿ\D���D�?\D�
Dп
D� �D�A�Dр�Dѿ�D��
D�>fD��D���D�  D�?\D��Dӿ\D���D�@ D�\D�� D� �D�@�DՀ Dտ\D��\D�?�DրRD�� D�  D�?\D�
D׿
D���D�@�D؀�D���D� �D�?�Dـ Dٿ�D��
D�?\DڀRD��RD��\D�?
DۀRD��RD���D�@�D܀RDܿ
D�  D�@�D݀RDݿ\D� RD�@RD��D��RD���D�>�D߀ D��RD���D�?�D��D�� D�  D�@RD� D�
D��\D�@�D� D�\D� �D�@�D� D�\D��
D�?
D� D�\D��
D�@RD値D���D� RD�@RD�RD�\D�  D�@RD��D�
D��
D�?
D��D���D�  D�?\D�
D�
D���D�@�D� D��RD� �D�?�D� D�� D��\D�@ D�RD���D� RD�?�D��D�� D� �D�@�DD�� D��
D�?�D� D��RD� RD�@RD��RD���D� �D�@RD��D�\D���D�@ D� D���D�  D�?
D�RD��RD��
D�?\D� D�� D� �D�@�D���D��HD��D�@�D��RD���D���D�@�D��RD���D��
D�>�D�� D���D��\D�?�D�~�D���D� �D�@ D�� D���D� RD�@ D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ɺA�(�A�u%A�@A�*�A�_A�	�A�SA��A��cA��A��xA���A���A��lA���A��A���A���A��;A���A��cA��/A��iA��A��A��]A��"A��
A��]AȫkA�A A���A���A��0A���A�Aǽ<Aǻ0AǲaAǮ�AǥzAǜAǗ�AǌJA�|PA�q�A�oiA�v�A�v�A�uZA�w�A�wfA�z�Aǋ�AǏ�A�r�A�1A�_AÅA��+A��A��oA�.IA��A�#:A���A��fA���A��)A�k�A�چA���A��A���A��A��'A�jKA� 'A�{A�L�A�oA��A���A�ɆA�A�C�A�ffA�v`Ax��Am��Ai�}Af��Af7�Ad#:Acm]Aby>A_�A]�gAWo�AOn/AI�AH�TAFƨAC4A<��A;��A;hsA:zxA9"�A8��A7D�A5e,A3�&A2S�A1MA/�A-TaA'�KA$>BA"��A!E�A!y�A!.IA $�A�A��AzA��A�BA�zA��A �	A �<A!%A!G�A!��A!��A!�7A �+A �)A �XA e�A�2AYKA�A��AMA8�Ad�A�Ab�A��A�oAV�A+A�7A%FAu%A��A��A%�A�\A�.A��A�KA�A��A��ArGA�AhsA�}A��A��ACA��A��A3�A��A�zA��AbA�UAzA.IAoA
E�A	A�A	�A�)A�6A��A+kA�HA�A_pA�A��A:�A�#A�A�xAp�A�A��A�qA�@A�	A/A�|A�A\)A1'A�AsA#�A�A �XA �A U2@���@��8@��h@��u@��A@�-�@�,�@��?@���@��D@��M@�b�@�N<@�<6@���@�+k@��h@���@���@��F@�3�@���@���@��@�U2@��#@�j�@��@�z@��@�u�@��@�c�@�@�;d@@�?�@�\)@쀝@��@�R�@�W?@�o@��@禵@�X@�)_@��m@�0U@�k�@�V@�o @�͟@�(�@�33@�u�@��@ߖS@��@ޥz@���@�|�@�e,@�ی@�Xy@�@�X@ڵ�@ڒ�@ڇ�@�PH@�~@���@٘�@�~(@��T@���@׻0@ץ@ו�@ׁ@�e�@�4@�3�@�Ɇ@Ӫ�@�zx@�Vm@�9�@�@���@���@Ҷ�@�Z@ѶF@�"�@��"@�ȴ@�B[@��@��j@ϕ�@�Y�@��@ι�@�|�@�*�@��@͕�@�ߤ@̅�@�0U@���@˭C@�.I@ʎ�@�Ta@ɩ�@��@�oi@�b@�Y�@��@Ɠu@�A�@ų�@��@�C�@�|�@�ѷ@��@���@�,�@��v@���@�j@�_@��@�O@�S@���@�~�@�d�@�<�@��.@���@�Vm@���@��O@�s�@��@��@�j@��	@�y>@��@�o�@��@�x@��H@�v`@��@�҉@�j@�)�@��@��"@��P@�ں@��[@��X@��@�z�@�b@�u�@��c@�q�@�\�@��@���@�dZ@�#�@��8@�ff@���@���@�a�@��@�bN@��@��'@�e�@��`@�d�@�e@���@�c�@�H�@�S�@�<6@���@�~�@��@��K@���@���@�|@��^@��@��k@�*0@�ߤ@���@�(�@��@��o@�@���@��"@�_p@�@�l"@�,=@���@��@�v`@���@��\@�PH@� �@���@��@��j@�$@�J�@� i@���@�r�@�+k@���@��K@���@�n/@��8@�h�@�6�@���@��h@�G�@��@��2@��@�J�@���@��@��@�A�@��@�(@��	@�;@��X@��@�l"@�e@���@�B�@��@�}V@��@���@�_p@�8@�'�@��|@���@�Z�@�R�@�;�@��@���@��>@�@���@�E9@�V@�@@��M@��e@�H@��]@�|@�H�@��p@�M�@�@��@���@���@��&@���@��{@�RT@�+�@�!-@�
=@��2@���@���@�3�@��X@�A @�%@��[@��'@�{�@�D�@�*�@��]@��
@�=�@��@��@��j@��z@��C@��~@�O@�9�@��@��@��[@���@�V�@� �@��&@�v`@�!-@��@��4@�u�@�xl@�Q�@�!@��*@�}�@�Y�@�)_@���@�I�@��]@��@��@��@@�^�@�,�@��P@��@��@��@��@���@�i�@�!�@��
@�t�@�G�@�"�@��@��@��]@��R@��@�4n@���@��@�w2@�/@��@�(@���@�L0@�@�{@
=@~��@~xl@~H�@~M�@~R�@~_@}ԕ@}B�@}m]@}��@}O�@}+@}�@}�@|��@|��@|`�@|�@{��@{&@z{�@zC�@y��@yJ�@x�p@x'R@x�@w�a@v�@v!�@u�N@u�X@ux�@u5�@t�	@t/�@s��@sb�@s�@r�!@r($@q�~@p�E@p�o@p  @o]�@n�@n��@m�^@me,@m+@l�4@lg8@kخ@kb�@k"�@j��@j�@i�S@iX@i@h��@hh�@g��@g�@g�@f��@f�1@f?@f�@e�@dm�@c�@ct�@c1�@b�<@bn�@b�@a�C@ap�@a^�@`Ĝ@`K^@_�+@_y�@_�@^��@^:*@]�t@]rG@]k�@\�p@\��@\]d@\ �@[s@Z��@ZZ�@Z	@Y�N@Y`B@Y�@Xj@XN�@XC-@Wخ@Wt�@W i@V��@V^5@V �@U��@U��@U=�@TĜ@T��@TS�@S�a@S�f@SK�@Rں@R�\@R?@R �@Q��@Q�d@Q-w@P�5@P�K@P�v@P��@P�@P�O@Pw�@O�@O�k@Ov`@O;d@O�@N�@N�]@N�@N� @N4@M�N@M�@Mzx@L��@Lr�@LM@KP�@J�@J�@J�@J�@I�@If�@H��@H�p@H��@H�@H�@HV�@H!@G�W@G��@G{J@GC�@F�!@F=q@Es�@E(�@D�f@DM@C�q@C8@B��@Bl�@A��@A��@A��@A\�@A5�@@�e@@M@?خ@?��@?\)@?A�@? i@>s�@=��@=}�@=!�@<�p@<�I@<Q�@<2�@<G@;��@:��@:�<@:Q@:4@:@9�@9��@9��@9�"@9Vm@8��@8�@7�K@7��@7�@@7��@7S�@6��@6�@6�+@6�@5hs@5:�@4�E@4S�@4G@3ƨ@3~�@3$t@2�1@2�@1�o@1�9@1��@1��@1|@1@@0��@0M@0M@/
=@.��@.��@.�L@.�1@.z@-�@-Vm@-5�@-�@,�v@,��@,��@,c�@,4n@+�]@+�}@+��@+��@+��@+j�@+33@*�L@*6�@)�Z@)�j@)��@)T�@);@(ѷ@(��@(Xy@'�r@'ݘ@'� @'� @'�}@'�}@'��@'��@'�a@'��@'��@'��@'��@&ȴ@&GE@&-@&&�@&
�@%�@%��@%�@%�'@%hs@%A @%#�@%@$�@$ѷ@$y>@$Xy@#�@#�g@#��@#v`@#=@#'�@#@"�H@"�X@"��@"~�@"p;@"ff@"a|@"C�@":*@"e@!��@!��@!k�@!5�@!;@ �P@ �	@ �/@ ��@ �4@ �@ r�@ 'R@ �@�@�F@�$@a@�@�@kQ@O@��@�@�@�'@e,@L�@A @?}@?}@8�@+�@�?@N�@�@x@>�@��@�s@�x@h
@M�@�D@�M@j@5�@+@��@��@`�@�@��@��@a@F�@"�@��@��@�@�z@k�@@�P@�[@e�@��@iD@F�@6z@�@�@��@��@��@p;@8�@	@�D@�@��@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ɺA�(�A�u%A�@A�*�A�_A�	�A�SA��A��cA��A��xA���A���A��lA���A��A���A���A��;A���A��cA��/A��iA��A��A��]A��"A��
A��]AȫkA�A A���A���A��0A���A�Aǽ<Aǻ0AǲaAǮ�AǥzAǜAǗ�AǌJA�|PA�q�A�oiA�v�A�v�A�uZA�w�A�wfA�z�Aǋ�AǏ�A�r�A�1A�_AÅA��+A��A��oA�.IA��A�#:A���A��fA���A��)A�k�A�چA���A��A���A��A��'A�jKA� 'A�{A�L�A�oA��A���A�ɆA�A�C�A�ffA�v`Ax��Am��Ai�}Af��Af7�Ad#:Acm]Aby>A_�A]�gAWo�AOn/AI�AH�TAFƨAC4A<��A;��A;hsA:zxA9"�A8��A7D�A5e,A3�&A2S�A1MA/�A-TaA'�KA$>BA"��A!E�A!y�A!.IA $�A�A��AzA��A�BA�zA��A �	A �<A!%A!G�A!��A!��A!�7A �+A �)A �XA e�A�2AYKA�A��AMA8�Ad�A�Ab�A��A�oAV�A+A�7A%FAu%A��A��A%�A�\A�.A��A�KA�A��A��ArGA�AhsA�}A��A��ACA��A��A3�A��A�zA��AbA�UAzA.IAoA
E�A	A�A	�A�)A�6A��A+kA�HA�A_pA�A��A:�A�#A�A�xAp�A�A��A�qA�@A�	A/A�|A�A\)A1'A�AsA#�A�A �XA �A U2@���@��8@��h@��u@��A@�-�@�,�@��?@���@��D@��M@�b�@�N<@�<6@���@�+k@��h@���@���@��F@�3�@���@���@��@�U2@��#@�j�@��@�z@��@�u�@��@�c�@�@�;d@@�?�@�\)@쀝@��@�R�@�W?@�o@��@禵@�X@�)_@��m@�0U@�k�@�V@�o @�͟@�(�@�33@�u�@��@ߖS@��@ޥz@���@�|�@�e,@�ی@�Xy@�@�X@ڵ�@ڒ�@ڇ�@�PH@�~@���@٘�@�~(@��T@���@׻0@ץ@ו�@ׁ@�e�@�4@�3�@�Ɇ@Ӫ�@�zx@�Vm@�9�@�@���@���@Ҷ�@�Z@ѶF@�"�@��"@�ȴ@�B[@��@��j@ϕ�@�Y�@��@ι�@�|�@�*�@��@͕�@�ߤ@̅�@�0U@���@˭C@�.I@ʎ�@�Ta@ɩ�@��@�oi@�b@�Y�@��@Ɠu@�A�@ų�@��@�C�@�|�@�ѷ@��@���@�,�@��v@���@�j@�_@��@�O@�S@���@�~�@�d�@�<�@��.@���@�Vm@���@��O@�s�@��@��@�j@��	@�y>@��@�o�@��@�x@��H@�v`@��@�҉@�j@�)�@��@��"@��P@�ں@��[@��X@��@�z�@�b@�u�@��c@�q�@�\�@��@���@�dZ@�#�@��8@�ff@���@���@�a�@��@�bN@��@��'@�e�@��`@�d�@�e@���@�c�@�H�@�S�@�<6@���@�~�@��@��K@���@���@�|@��^@��@��k@�*0@�ߤ@���@�(�@��@��o@�@���@��"@�_p@�@�l"@�,=@���@��@�v`@���@��\@�PH@� �@���@��@��j@�$@�J�@� i@���@�r�@�+k@���@��K@���@�n/@��8@�h�@�6�@���@��h@�G�@��@��2@��@�J�@���@��@��@�A�@��@�(@��	@�;@��X@��@�l"@�e@���@�B�@��@�}V@��@���@�_p@�8@�'�@��|@���@�Z�@�R�@�;�@��@���@��>@�@���@�E9@�V@�@@��M@��e@�H@��]@�|@�H�@��p@�M�@�@��@���@���@��&@���@��{@�RT@�+�@�!-@�
=@��2@���@���@�3�@��X@�A @�%@��[@��'@�{�@�D�@�*�@��]@��
@�=�@��@��@��j@��z@��C@��~@�O@�9�@��@��@��[@���@�V�@� �@��&@�v`@�!-@��@��4@�u�@�xl@�Q�@�!@��*@�}�@�Y�@�)_@���@�I�@��]@��@��@��@@�^�@�,�@��P@��@��@��@��@���@�i�@�!�@��
@�t�@�G�@�"�@��@��@��]@��R@��@�4n@���@��@�w2@�/@��@�(@���@�L0@�@�{@
=@~��@~xl@~H�@~M�@~R�@~_@}ԕ@}B�@}m]@}��@}O�@}+@}�@}�@|��@|��@|`�@|�@{��@{&@z{�@zC�@y��@yJ�@x�p@x'R@x�@w�a@v�@v!�@u�N@u�X@ux�@u5�@t�	@t/�@s��@sb�@s�@r�!@r($@q�~@p�E@p�o@p  @o]�@n�@n��@m�^@me,@m+@l�4@lg8@kخ@kb�@k"�@j��@j�@i�S@iX@i@h��@hh�@g��@g�@g�@f��@f�1@f?@f�@e�@dm�@c�@ct�@c1�@b�<@bn�@b�@a�C@ap�@a^�@`Ĝ@`K^@_�+@_y�@_�@^��@^:*@]�t@]rG@]k�@\�p@\��@\]d@\ �@[s@Z��@ZZ�@Z	@Y�N@Y`B@Y�@Xj@XN�@XC-@Wخ@Wt�@W i@V��@V^5@V �@U��@U��@U=�@TĜ@T��@TS�@S�a@S�f@SK�@Rں@R�\@R?@R �@Q��@Q�d@Q-w@P�5@P�K@P�v@P��@P�@P�O@Pw�@O�@O�k@Ov`@O;d@O�@N�@N�]@N�@N� @N4@M�N@M�@Mzx@L��@Lr�@LM@KP�@J�@J�@J�@J�@I�@If�@H��@H�p@H��@H�@H�@HV�@H!@G�W@G��@G{J@GC�@F�!@F=q@Es�@E(�@D�f@DM@C�q@C8@B��@Bl�@A��@A��@A��@A\�@A5�@@�e@@M@?خ@?��@?\)@?A�@? i@>s�@=��@=}�@=!�@<�p@<�I@<Q�@<2�@<G@;��@:��@:�<@:Q@:4@:@9�@9��@9��@9�"@9Vm@8��@8�@7�K@7��@7�@@7��@7S�@6��@6�@6�+@6�@5hs@5:�@4�E@4S�@4G@3ƨ@3~�@3$t@2�1@2�@1�o@1�9@1��@1��@1|@1@@0��@0M@0M@/
=@.��@.��@.�L@.�1@.z@-�@-Vm@-5�@-�@,�v@,��@,��@,c�@,4n@+�]@+�}@+��@+��@+��@+j�@+33@*�L@*6�@)�Z@)�j@)��@)T�@);@(ѷ@(��@(Xy@'�r@'ݘ@'� @'� @'�}@'�}@'��@'��@'�a@'��@'��@'��@'��@&ȴ@&GE@&-@&&�@&
�@%�@%��@%�@%�'@%hs@%A @%#�@%@$�@$ѷ@$y>@$Xy@#�@#�g@#��@#v`@#=@#'�@#@"�H@"�X@"��@"~�@"p;@"ff@"a|@"C�@":*@"e@!��@!��@!k�@!5�@!;@ �P@ �	@ �/@ ��@ �4@ �@ r�@ 'R@ �@�@�F@�$@a@�@�@kQ@O@��@�@�@�'@e,@L�@A @?}@?}@8�@+�@�?@N�@�@x@>�@��@�s@�x@h
@M�@�D@�M@j@5�@+@��@��@`�@�@��@��@a@F�@"�@��@��@�@�z@k�@@�P@�[@e�@��@iD@F�@6z@�@�@��@��@��@p;@8�@	@�D@�@��@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BQB�B�B�B�BgBMBMB�B�B�B�B�B�B�B�B�BgB2BMBBgBgB�B�B�B�B�BBBJB�BtB�BB�B�B�B�B�BYB�BmB3BGBGB�B�B�B�B�B�B
XBBhBBg�B�/B	�B	̘B
��B
�4B
�nB
�B
�B�B)_B�B�B
=BBB�B*B*�B
�B
�B
�+B
��B
��B
��B
��B
�SB
u%B
a�B
/ B
uB	�*B	�B	z�B	G+B	2�B	-B	G�B	DgB	B�B	>�B	33B	(�B	SB	�B��B�$B��B�B�B�HB	aB	�B	EB	YB	�B	B	B	_B	�B	^B	�B�IB��B	�B	]B	'�B	,=B	%�B	$tB	%`B	,�B	<�B	LB	P�B	Y�B	��B	��B	��B	�-B	�B	�(B	��B	�KB	��B	�tB	�tB	�?B	ňB	ŢB	�mB	�MB	ǔB	ʦB	��B	ʦB	�VB	�}B	ѷB	��B	ևB	�mB	��B	��B	�kB	�qB	�B	چB	�yB	ּB	�
B	�QB	ڠB	�VB	�-B	�nB	�zB	�B	�2B	�B	�B	�B	�B	��B	�XB	�B	�B	�yB	�B	�B	�B	�WB	��B	�B	�B	��B	�CB	�B	�B	�B	�!B	�oB	��B	�B	��B	��B	�B	�B	�?B	�?B	�B	��B	��B	��B	�+B	��B	�?B	��B	�B	�B	�%B	�%B	�?B	��B	��B	��B	�tB	�ZB	�%B	��B	�B	�B	��B	��B	��B	�?B	�%B	��B	�nB	��B	��B	�GB	�B	�oB	�oB	��B	�B	��B	��B	��B	�!B	��B	�UB	��B	�OB	�B	�B	�IB	��B	��B	�B	�B	��B	�WB	�B	�B	�B	�B	�kB	�QB	�B	��B	�B	�B	�B	�0B	�B	��B	�mB	�B	�B	�ZB	�ZB	�ZB	�B	�B	��B	�B	�B	�@B	�@B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�fB	�B	�B	�fB	�LB	�LB	��B	�zB	�LB	�XB	��B	�B	�B	�sB	�B	�XB	�>B	��B	��B	��B	�*B	�B	�0B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�eB	��B	�B	��B	�B	�B	��B	�B	��B	�B	��B	�=B	�qB	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�;B	�B	��B	�;B	�B	�!B	�oB	�B	��B	�B	��B	�B	�B	�B	�AB	��B	�B	�B	�B	�B	�B	��B	�B	�B	�hB	�nB	�?B	��B	�+B	�`B	�`B	�FB	�+B	�B	�FB	�+B	�FB	��B	��B	��B	��B	��B	��B	�8B	��B	�lB	�B	�fB	��B	��B	��B	�2B	��B	�fB	��B	��B	��B	��B	�B	�dB	�6B	��B	��B	��B	��B	��B	�cB	��B	��B	��B	��B
  B
  B
 B
 �B
gB
B
+B
%B
�B
	RB
^B
xB
�B
�B
�B
�B
�B
�B
B
�B
�B
VB
�B
BB
B
�B
B
�B
�B
�B
B
	�B
	�B
	�B
)B
�B
�B
�B
^B
�B
�B
jB
jB
B
VB
�B
�B
�B
�B
�B
hB
hB
hB
�B
oB
�B
B
B
,B
�B
�B
MB
�B
�B
sB
�B
�B
�B
B
B
B
B
�B
�B
	B
#B
=B
�B
B
)B
B
�B
B
�B
~B
�B
B
�B
5B
5B
�B
�B
�B
OB
;B
pB
 'B
 vB
 �B
 vB
 �B
 �B
 BB
 'B
 �B
 �B
 �B
 BB
 'B
 �B
"�B
"�B
"�B
#TB
#�B
$B
$tB
#�B
#TB
$�B
$�B
$�B
$�B
$�B
%,B
%FB
%,B
%�B
%�B
%�B
&LB
&�B
&�B
'�B
'�B
'�B
'mB
&�B
'8B
)B
)�B
*�B
*�B
*�B
*B
)�B
)_B
(�B
)B
)yB
)B
)�B
)�B
*eB
*�B
+B
+QB
+�B
+�B
+�B
,=B
,WB
,�B
,qB
,�B
,WB
,=B
,B
,"B
,B
,=B
,qB
+kB
*�B
*B
)�B
)�B
)�B
)_B
)B
)DB
)*B
)B
)B
)yB
)yB
)�B
)�B
+�B
+�B
-B
/�B
1vB
1�B
1�B
1�B
1[B
1[B
2�B
3�B
4B
5%B
5�B
5�B
5�B
5?B
4�B
4�B
5%B
5%B
5?B
5ZB
5�B
5�B
5�B
5�B
5�B
6�B
6�B
7LB
7LB
7�B
88B
8�B
9$B
9$B
9�B
:B
:DB
:DB
:�B
:�B
:�B
;0B
;JB
;�B
;�B
;�B
<PB
<�B
=VB
=VB
=�B
>(B
>�B
?�B
@iB
@iB
@�B
AoB
AUB
A�B
B�B
B�B
B�B
B�B
B�B
CB
C�B
C{B
C{B
CaB
CGB
C�B
C�B
DgB
D�B
EB
ESB
E�B
E�B
E�B
F�B
G+B
G�B
G�B
HKB
IB
I�B
J#B
J=B
JrB
J�B
J�B
KxB
KxB
KDB
K�B
K�B
L0B
LJB
LdB
L�B
L�B
L�B
MB
M6B
M6B
M�B
M�B
M�B
M�B
N<B
NpB
N�B
N�B
N�B
N�B
OBB
OBB
OBB
OBB
OBB
OBB
O\B
OvB
O�B
O�B
PB
PHB
P}B
P}B
PbB
PHB
P�B
P�B
P�B
Q B
QB
QhB
Q�B
Q�B
RTB
R�B
SB
S@B
S�B
S�B
TFB
TaB
TaB
TaB
TaB
T{B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UgB
VSB
VSB
VSB
W$B
WsB
W�B
W�B
XB
X�B
XyB
X�B
X�B
X�B
Y1B
Y�B
Y�B
ZB
ZQB
ZQB
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\)B
\)B
\)B
\�B
]B
]IB
]�B
^B
^B
^5B
^OB
^OB
^�B
^�B
_B
`vB
`�B
`�B
`�B
`�B
aB
a|B
abB
abB
a�B
a�B
a�B
b�B
c B
cTB
c�B
c�B
d&B
d�B
d�B
d�B
d�B
eB
d�B
eB
eFB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
h
B
h
B
hXB
hsB
h�B
h�B
h�B
iB
i*B
i_B
i�B
i�B
i�B
i�B
j0B
jKB
j�B
j�B
j�B
kB
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
m]B
mwB
mwB
mwB
m�B
m�B
m�B
m�B
n/B
n/B
nIB
ncB
ncB
n}B
n�B
n�B
o5B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p;B
p!B
p!B
p;B
p;B
pUB
poB
p�B
p�B
qB
qAB
q'B
q'B
qAB
q[B
qvB
q[B
q�B
q�B
q�B
q�B
rB
r-B
rGB
raB
rGB
sB
sMB
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
s�B
s�B
t9B
t�B
t�B
utB
u�B
u�B
u�B
v+B
v+B
vFB
v�B
v�B
wB
wLB
w2B
wfB
w�B
w�B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y�B
zB
z�B
z�B
z�B
z�B
{0B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}VB
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BqB
BB�B�BgBMBMB�B�B�B�B�B�B�B�B�BgB2BMBBgBgB�B�B�B�BB�B�B�BB�B�BB�B�B�B�B�BtB�B�BMBaBGB�B�B�B�B�B�B
XB\B�BBk�B��B	�B	�1B
�ZB
�:B
�`B
�B
��B�B-]B�B�BB@BB0�B3�B
��B
�B
��B
�}B
��B
�;B
�kB
�B
y�B
jKB
72B
fB	��B	�-B	�B	K�B	5�B	.�B	I�B	E�B	D�B	B�B	7�B	1'B	;B	�B��B��B�B��B��B��B	B		B	KB	fB	�B	3B	%B		RB	�B	�B	,B�|B�5B	�B	CB	(sB	-]B	%�B	$�B	%�B	,�B	<�B	LB	P}B	X�B	�'B	�rB	�hB	��B	�5B	�wB	�aB	ȀB	�EB	��B	�+B	��B	�%B	�?B	�?B	ňB	ȚB	�^B	˒B	�^B	οB	��B	� B	ԕB	�$B	�YB	ٚB	�kB	�=B	�xB	�dB	��B	ٚB	��B	��B	�=B	�WB	�'B	��B	�tB	�B	��B	�B	�B	�$B	�>B	�>B	�>B	�B	�DB	�B	��B	�B	�B	�6B	�wB	�B	��B	��B	��B	��B	��B	� B	��B	�B	�'B	�B	��B	�MB	�9B	�nB	��B	�tB	�ZB	�?B	�%B	�`B	�B	��B	��B	��B	�B	��B	�tB	�ZB	�tB	��B	�tB	�tB	�FB	��B	�tB	�ZB	�?B	��B	��B	�B	�B	��B	�ZB	�?B	�%B	��B	�%B	�nB	�B	��B	��B	�B	�;B	��B	�OB	�iB	�!B	�oB	��B	�B	�UB	��B	�B	�5B	�B	�IB	�]B	�B	�)B	�B	�B	��B	�B	�WB	�B	��B	�B	�6B	�6B	�6B	�0B	�kB	��B	�*B	�_B	�
B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�tB	�B	�,B	�B	�B	��B	�,B	�B	�FB	�`B	�B	�B	�B	�B	�B	�fB	�B	�fB	�LB	�8B	��B	�B	��B	�B	�B	�B	�sB	�sB	�>B	�XB	�*B	�_B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�6B	��B	�B	��B	�B	�QB	�kB	�kB	�B	�WB	�B	��B	�]B	�]B	�]B	�CB	�wB	�cB	�IB	�}B	�5B	�B	�;B	�oB	�;B	�!B	�oB	�UB	�oB	�B	��B	�B	�!B	�!B	��B	�AB	�[B	�B	�B	�GB	�aB	��B	��B	��B	�MB	�B	��B	��B	�B	��B	��B	�zB	��B	��B	�zB	�`B	�zB	��B	�FB	�`B	��B	��B	��B	��B	�2B	��B	�lB	��B	��B	�lB	��B	�B	��B	�B	��B	��B	��B	�$B	�B	�B	��B	�DB	��B	��B	��B	��B	�"B	��B	��B	��B	�HB	��B	��B
  B
 B
 B
 4B
 �B
�B
+B
zB
YB
�B
	�B
xB
�B
�B
�B
�B
B
JB
B
PB
�B
B
�B
(B
vB
BB
BB
vB
VB
�B
B
~B
	�B
	�B
	�B
^B
JB
�B
�B
�B
B
PB
�B
�B
VB
�B
B
�B
B
�B
�B
�B
�B
�B
�B
�B
&B
&B
FB
aB
�B
B
�B
�B
�B
�B
EB
1B
1B
1B
�B
QB
QB
�B
	B
#B
=B
WB
�B
)B
]B
]B
�B
/B
�B
�B
�B
jB
�B
jB
�B
�B
�B
B
jB
VB
pB
 BB
 �B
 �B
 �B
 �B
 �B
 \B
 BB
 �B
 �B
 �B
 �B
 \B
!-B
"�B
"�B
# B
#nB
$B
$@B
$�B
$tB
#�B
$�B
$�B
$�B
$�B
%B
%FB
%`B
%FB
%�B
%�B
%�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
&�B
'mB
)DB
)�B
*�B
*�B
+B
*�B
*B
)�B
(�B
)*B
)�B
)DB
)�B
)�B
*B
*�B
+6B
+QB
+�B
+�B
,"B
,qB
,�B
,�B
,�B
,�B
,qB
,WB
,"B
,=B
,WB
,�B
,�B
+�B
+B
*0B
*B
*0B
)�B
)�B
)DB
)yB
)DB
)*B
)*B
)yB
)yB
)�B
*B
+�B
+�B
-)B
/�B
1�B
1�B
1�B
1�B
1vB
1�B
2�B
3�B
4TB
5ZB
5�B
6B
5�B
5tB
5B
4�B
5ZB
5�B
5tB
5ZB
5�B
5�B
5�B
6B
6FB
6�B
7B
7fB
7�B
7�B
8lB
9	B
9rB
9XB
9�B
:DB
:xB
:�B
:�B
:�B
;0B
;0B
;B
;�B
;�B
<B
<�B
<�B
=qB
=qB
=�B
>BB
?B
@ B
@OB
@�B
A B
A�B
A�B
A�B
B�B
B�B
C-B
CB
CB
C-B
C�B
C�B
C�B
C{B
C{B
C�B
C�B
D�B
EB
ESB
E�B
E�B
E�B
E�B
F�B
G_B
G�B
HB
H�B
I7B
J#B
JXB
JXB
J�B
J�B
KB
K�B
K�B
KxB
K�B
LB
LJB
LJB
L~B
L�B
L�B
MB
M6B
MPB
MjB
M�B
M�B
M�B
NB
NpB
N�B
N�B
N�B
N�B
N�B
O\B
OBB
OBB
OBB
O(B
OBB
OvB
O�B
O�B
PB
P.B
PHB
P}B
P}B
P}B
PHB
P�B
Q B
Q B
QB
QNB
Q�B
Q�B
R:B
R�B
R�B
S@B
S[B
S�B
S�B
TaB
T{B
TaB
T{B
T�B
T{B
T�B
T�B
T�B
T�B
T�B
UB
UgB
U�B
V�B
VmB
V�B
WYB
W�B
W�B
W�B
XEB
X�B
X�B
X�B
X�B
X�B
YB
Y�B
ZB
ZB
Z�B
ZkB
Z�B
[=B
[�B
[�B
[�B
\B
\B
\CB
\)B
\]B
\�B
]/B
]~B
^B
^B
^B
^OB
^OB
^�B
^�B
_B
_VB
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a|B
a�B
a�B
a�B
a�B
b�B
c:B
cnB
c�B
dB
dZB
d�B
eB
d�B
eB
e,B
eB
eFB
ezB
e�B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
gRB
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
i*B
i*B
iDB
i�B
i�B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
j�B
kB
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
mB
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
nIB
nIB
ncB
n}B
n}B
n�B
n�B
o B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p!B
p;B
p!B
p;B
p;B
pUB
poB
p�B
p�B
p�B
q'B
qAB
qB
qAB
q[B
qvB
qvB
q[B
q�B
q�B
q�B
rB
r-B
rGB
raB
r|B
r|B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
tnB
t�B
u?B
u�B
u�B
u�B
vB
v`B
vFB
vzB
v�B
wB
w2B
wfB
wLB
w�B
w�B
xB
xlB
x�B
x�B
y	B
y	B
y$B
y>B
yrB
zB
z*B
z�B
z�B
z�B
z�B
{dB
|PB
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}<B
}<B
}qB
}�B
}�B
}�B
~B
}�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��p<F?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202004190048382020041900483820200419004838202207271135282022072711352820220727113528202207271537482022072715374820220727153748  JA  ARFMdecpA30a                                                                20200408033818  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200408033828  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200408033829  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200408033829  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200408033830  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200408033830  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200408033830                      G�O�G�O�G�O�                JA  ARUP                                                                        20200408035426                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200409000000  CF  PSAL_ADJUSTED_QC@2�\@}p�G�O�                JM  ARCAJMQC2.0                                                                 20200418154838  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200418154838  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023528  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063748  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                