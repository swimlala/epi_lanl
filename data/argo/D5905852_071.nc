CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-14T18:40:25Z creation;2021-01-14T18:40:27Z conversion to V3.1;2022-08-17T01:55:19Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210114184025  20220818091506  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA  A30_8420_071                    2C  D   APEX                            8420                            2.11.2                          846 @�V��ր1   @�V�HpC @.U'�0�c
�-�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A>ffA`  A���A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�ffB�ffB�  B�  B�  B�  B�  B˙�B���B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C� C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z�@�  @�\)@��A�A?
=A`z�A�ffA�{A��A��A��
A�(�A�{A�{B   B  B�B
=B   B(
=B033B8  B@
=BH
=BO��BW�B_��Bh
=Bo�
Bx
=B�B���B���B���B�B�
=B��B���B�  B�\B�G�B�k�B�p�B�\)B��fB�  B��B���B�{B˅B���B�B�k�B���B��B���B���B�B�  B�B���B���B���C�RC  C�C�HC	��C��CC  C��C�qC�C�C�CC  C�qC"  C#�RC%��C'�qC*C,C-�qC/��C1��C4�C6  C7��C:  C<C>�C@  CA��CD  CF�CHCI��CK��CM�qCO�qCR  CT  CV  CX�CZ�C\�C^C_�qCb�CdCe�qCg��Ci��Cl  Cn�Co��Cq�RCtCv�Cw��Cz  C|C~  C�qC���C�  C��C�HC���C���C���C��qC��C���C��)C���C��qC��C��C�HC��qC��C��C�  C�HC�  C�HC�HC���C���C�  C��C��C�HC���C���C��C�  C�HC��C�  C�HC��C��C�HC�  C��C�HC�HC�  C���C��C��C�  C���C��qC��qC�HC�  C��)C���C�HC��C�C��C��C��C��qC���C�HC��C��C���C���C��qC��C�  C���C��C���C���C�  C��C���C���C�HC�HC�HC�HC�  C���C���C�HC��C��C�  C���C�fC��C�  C�HC���C�HC�HC�HC���C��qC�  C��C�HC�  C���C��)C���C���C��qC�  C�  C�HC���C��qC��qC���C�HC�  C��qC��qC��C�  C��)C�  D HD � DHD��D  D��D�\D~D �D�HD�\D}qD�\D�HD��D� DHD\D�\D	� D
  D
��D �D~D�D� D�D�HD�\D~�D  D��D �D� D�\D\D  D� D�\D}qD��D�HD �D~�D��D�HD�D��D  D� DHD��D�D�HD  D��DHD��DHD��D �D�HDHD��D  �D ��D!  D!��D"�D"��D# �D#��D$�D$��D%  D%}qD%�D&~�D&��D'\D(  D(\D(��D)~D)��D*~�D*�\D+\D,  D,��D-  D-��D-�\D.� D/HD/� D/�\D0��D1 �D1��D2  D2\D3  D3�HD4 �D4�HD5 �D5~�D5�\D6��D7 �D7��D8HD8\D8��D9~�D9�D:~D:��D;~�D<  D<��D=�D=\D=�\D>\D>�\D?� D?�qD@~�D@�\DA~�DA�DB}qDB�qDC|�DC��DD}qDD�qDE}qDE�DF~DF�\DG��DH �DH� DI�DI��DJ  DJ� DJ�\DK~�DK�DL� DL��DM~�DM��DN~DO  DO�HDP3DP�{DQ �DQ��DRHDR� DS �DS��DS�\DT~�DT�\DU~�DU��DV��DW �DW� DX �DX� DX��DY~DY�qDZ~DZ�D[~D[�\D\��D]  D]~�D]�qD^\D^�\D_~�D` �D`��D`��Da� Db  Db~�Db�\Dc� Dd �Dd\De  De�HDe�\Df~�Df��Dg\Dg�\Dh� DiHDi��DjHDj��Dk�Dk��DlHDl~�Dl��Dm~�Dn  Dn� Dn��Do~Dp  Dp��Dq  Dq~Dq�Dr\Ds  Ds�HDt  Dt\DuHDu� Dv  Dv� Dv�qDw~Dx  Dx~�Dx�Dy~�Dz  Dz��D{�D{��D{�D|~D|�D}\D~�D~�HD  D\D��D�?
D�~�D��
D���D�@RD��HD���D� RD�AHD��D���D� �D�@�D��D���D�HD�AHD���D���D� �D�@ D�� D��RD�  D�@ D���D�� D�  D�@RD��RD��RD���D�?\D��D��RD���D�?\D�� D���D� RD�@�D�� D��\D�  D�@RD��RD���D�  D�?\D��RD��RD���D�?�D�\D��\D� RD�@ D�� D��RD���D�?\D�\D���D� RD�@RD�
D�� D� �D�?�D��D�� D� �D�@�D�� D���D���D�@ D���D���D� RD�@�D���D��HD� RD�?�D��D��\D���D�@ D�
D���D� RD�?�D�� D���D��\D�?�D��D��RD� �D�@�D�� D��
D���D�?�D�\D���D���D�?\D���D���D���D�?\D��D��\D�  D�@�D�� D���D�  D�@RD�� D���D��\D�?\D��D�� D�  D�AHD��RD��\D� RD�@ D�� D��RD� RD�AHD���D���D� RD�?\D��RD���D���D�@ D���D��HD� �D�@RD��D��
D��
D�?\D�\D���D� RD�@�D�\D���D���D�@RD���D�� D��\D�@RD��D��\D��
D�?
D��RD�� D�  D�@RD��D��RD��D�A�D�� D���D� �D�@ D�� D���D��\D�?\D�\D��\D�  D�@�D���D��RD��\D�?�D��RD���D���D�>�D�\D�� D�HD�@�D��D�� D� �D�@�D�� D���D��
D�?
D�\D���D� RD�AHD��RD��
D���D�@ D���D��HD� RD�?\D�\D���D� �D�AHD��RD��\D���D�?�D�� D���D� �D�@ D��D��
D��
D�?�D���D���D� �D�@ D��D��RD��
D�>�D��D���D� RD�@ D�� D���D� �D�@RD�\D��RD� RD�?�D�\D���D� �D�?�D�
D�� D�  D�@RDRD��RD��D�A�DÀ D�� D���D�?\D��D���D� �D�@ D�
D�� D� RD�?�D��Dƿ�D�  D�@�D��Dǿ�D� RD�@�D��Dȿ\D�  D�@ D�\D��RD� RD�@ Dʀ Dʿ�D��
D�?
Dˀ D�� D� �D�@ D�
D̾�D��
D�?\D̀RD��RD��\D�?
D�
Dο\D���D�@RDπ�D���D� RD�@�DЀ�D��RD� �D�@�DрRDѿ�D��\D�?
D�
Dҿ\D�  D�@�DӁ�D���D��\D�?�DԁHD�� D��
D�?\DՀ�Dտ\D���D�?\D�\Dֿ
D��\D�?\D��D�� D� RD�@�D؀�D�� D���D�?\Dـ D��RD� �D�@ D�\D�� D�  D�?
DۀRD���D��D�@�D��Dܾ�D��\D�@ D�
D�� D� �D�@RD�\D޿
D���D�?�D߀�D�� D��\D�>fD�\D�� D���D�?\D�~�D�� D��D�@RD�~fD�� D�HD�@�D〤D�
D��fD�?\D��D修D���D�@RD値D���D� �D�?\D�\D濮D�  D�@RD�RD�� D��
D�?\D耤D�� D��\D�?�D� D��RD�  D�?
D��D��RD��\D�?\D��D뿮D���D�?\D�\D�\D�  D�@�D�RD��RD���D�>�D�\D�� D��\D�@�D��D�� D� RD�@RD�
D�fD��\D�@�D�D�� D���D�?�D�RD�� D� RD�@�D� D�� D� �D�@RD� D��\D�  D�?�D�~�D��
D��
D�@ D��D��\D���D�?
D�\D��RD� �D�?�D��D�� D� �D�@RD��RD���D���D�?
D�~�D���D��
D�?\D�\D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�[#A�`A�`vA�a|A�bA�dZA�e`A�f2A�ffA�g8A�h�A�j�A�l�A�m�A�m)A�ncA�m�A�l�A�m�A�n�A�o5A�t�A�v+A�xA�y>A�w�A�v�A�v�A�u%A�tA�v�A�t�A�u�A�x�A�}�A�~�A�xlA�u%A�j�A�_;A�^5A�P}A�,�AȌ�A�(�A��dA�S�A�Q�A���A��KA���A���A���A���A�-�A�kQA���A��iA�JXA�ӏA�A�2�A��A��Ay\�Ar4�AnɆAi��AdU�AaQA^!�A])_A\�AZ�AXAU5�AR��AQ~AO�zAN��AL��AIƨAFOAAW�A=��A9M�A4��A3�0A2��A1�XA1"hA.�A,y>A,�A+-�A+*0A+��A,��A.$tA.�A.�A,�zA,5?A,�A,�A,�A+�A,A,�A,$A,0UA,0�A+��A)�'A(��A'��A&��A&��A&e�A&YKA&+A&��A'�A'A'�/A'�A%��A'!�A'�A'��A'OvA&C�A%A�A$��A#��A"�eA!eA �DA ѷA A AL0A�jA@A��A�[Ac AaA�9A�HA��A��A�A}VAL�AZA��A��A��A�A~�A/�A��As�A1A�_AR�A�AjA��A�{A�A��AϫA�7A"hA�A�{A�A[WA�EAB[Au�A�ATaAA�A&�A��A��AIRA
=A��A��AU�AH�A�A��A�A�hA\�A2aA�A�'A��A�:AffAA
dZA
*0A	��A	/�A	A�\A��A{JA�A��A�DA^5AB[A4nA�TAX�A	lAzA<6A�AA�tA�A�4A$tAԕA�:Ae�A/�A �|A �fA b�A @�~�@���@�(�@�x�@��X@�9X@�� @��@�O@��@��$@��@�\)@� \@��2@��u@�5?@��@@�U�@�֡@���@�h�@�خ@�}�@�=@�@��@��@���@�f@�z�@��@��@��@�Q@�@�V@��/@�J�@鐗@�!�@譬@艠@�=q@�+k@� �@��.@簊@��@���@��@�?}@���@�"�@��Z@��@�r�@�/�@��a@�@O@��'@���@�j�@�E�@��@۵t@��@��@�_@��@���@غ�@؎�@�YK@׆�@� \@��p@֣@�n�@�J#@�6�@Ӝ@��@�L0@�l�@з�@Ϲ�@�e,@�)_@ά�@��@��&@̓{@̧@��A@˃{@� \@���@�ff@�!@�ƨ@�s�@��,@ȣ@Ȉ�@ȅ�@�|�@�@��,@�
�@�w2@��@�H@�@.@�@�}V@�bN@�X@��z@��a@��t@���@�� @��}@�O@���@�m�@���@�z@��@��@�$�@��d@�U�@��@�ں@�Z@���@�C@���@��@���@���@�C�@�]�@�q@��{@��@�$�@�dZ@�u�@���@�F�@��@�Ov@��.@���@���@�T�@��I@�&�@���@���@���@�f�@�!-@��K@���@�U2@�#:@���@�-w@��@���@��'@���@�%�@��@�F@�+@�Ĝ@�N�@��&@�o�@�4@�S@���@��4@�h�@�H@��W@�x@��@��@���@��Y@�:�@���@���@�o@�Z�@��@�C-@��@���@��=@�!�@��M@��]@���@���@�W�@�.�@�  @��K@��@�+@��@���@��A@�<�@�@��&@�ƨ@�{J@�F�@��@��@�m�@�D�@��w@���@��{@�.I@��@�*�@���@��@��N@��V@�H�@�ȴ@�Xy@���@�33@���@�h
@�2�@��@��7@�f�@�o@��O@�l�@��@��@���@�e,@��@���@��O@�l"@�O@��@�zx@�@O@���@���@��@��@�~(@�6@��@�	@��@���@���@�%F@��b@�/�@���@�v`@�Q�@��@��@�z@�5?@��@��q@�o�@��@���@��'@���@�|�@�;�@��.@��@���@�&@��@���@�<�@��]@��j@��z@�a�@��s@�y>@�C-@��@��@���@���@���@�dZ@�"�@���@���@�?�@�!�@��@���@�N<@�,�@��@��s@��@�z@�d�@�J�@�M@���@�*0@���@��1@��@���@���@�x�@�/@��]@��@�`�@�3�@�W@�@@iD@=@~��@~O@}��@}8�@|��@|e�@{��@{��@{v`@{K�@{!-@z��@z^5@z.�@y��@y�@x��@x��@x�5@xی@x�@w�
@w�	@w+@v��@v��@v�@vq�@vJ@um]@u%@t�@t��@t�u@tu�@tN�@s�@sW?@r�1@rO@q�-@qa�@q@ptT@p(�@p�@o�&@o�V@o��@oC�@ns�@m��@m��@m^�@mIR@m4@mq@l��@l��@k�+@k�V@kE9@k
=@j��@j:*@j�@i�N@h|�@g��@g_p@gF�@f�@f҉@f�'@f�+@f�@e��@d��@d�4@d~(@d$@c�r@c��@c>�@b��@bL0@a�9@a��@`��@`~@_˒@_�@_'�@^�m@^��@^$�@]�j@]c�@]@\�@\�@\�o@\Xy@\>B@[�+@[b�@['�@Z�@Z��@Y��@Y*0@YV@X��@X�E@Xu�@X�@W�V@V��@V��@V�b@VTa@U�@Us�@U�@T��@TZ@T~@S��@S�q@S9�@R�}@Rq�@R1�@Q�M@Q�@P�@P~(@PH@P�@O��@O&@N�]@N��@N��@NQ@M��@M�M@Lی@Lz�@Kݘ@K~�@K6z@J��@Ji�@J�@I�z@Io @H�@H�u@G�a@G�P@G1�@F�]@F�F@FB[@E�z@E�=@Ec�@D�	@DS�@D(�@C��@CMj@C�@Bȴ@B��@B~�@B@�@A��@A^�@@�p@@PH@?��@?dZ@>�R@>~�@>R�@>Q@>Ov@>&�@=�@=F@<�@<�z@<-�@;��@;��@;4�@:�y@:��@:C�@:6�@9�D@9u�@9@8�@8oi@8>B@7�@7�q@7��@7�Q@7��@71�@6�]@6��@6ff@6e@5�'@4�f@4g8@4V�@49X@47@3خ@3��@3t�@3$t@2R�@1��@1��@1��@1J�@0ی@0g8@0'R@/�]@/�K@/s@/>�@/�@.�M@.�m@.q�@.M�@.:*@.O@.u@-�>@-�3@-�~@-8�@,��@,b@+��@+��@+W?@+'�@+o@+S@*�X@*��@*Q@*$�@)��@)�@)Y�@)=�@(�f@(Ĝ@(��@(�D@(tT@(K^@'�@'�@'�0@'~�@'C�@'�@&�c@&��@&i�@&{@&�@%��@%��@%��@%`B@%�@%�@%�@$�@$֡@$�4@$��@$|�@#��@#F�@#
=@"�!@"H�@"&�@"�@!�@!��@!��@!`B@!Dg@!�@ �5@ �@ PH@ *�@�m@�6@�[@�:@qv@Z�@��@z@@�@c�@A @�|@ی@��@V�@ �@�k@l�@9�@��@�B@h
@	@��@��@��@Y�@*0@��@�5@�/@��@�_@D�@�6@�$@P�@$t@�X@�+@C�@0U@�n@2a@��@��@�z@Q�@!@�@J#@�@��@�2@ߤ@�<@�F@c @E�@	@�T@��@w2@j@F@&�@��@��@r�@C-@  @�W@�a@�@�F@��@C@�<@�b@h
@C�@+k@��@��@o @B�@��@��@|�@  @��@a@1�@(@
��@
��@
p;@
_�@
Ta@
Ov@
=q@
�@	ϫ@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�[#A�`A�`vA�a|A�bA�dZA�e`A�f2A�ffA�g8A�h�A�j�A�l�A�m�A�m)A�ncA�m�A�l�A�m�A�n�A�o5A�t�A�v+A�xA�y>A�w�A�v�A�v�A�u%A�tA�v�A�t�A�u�A�x�A�}�A�~�A�xlA�u%A�j�A�_;A�^5A�P}A�,�AȌ�A�(�A��dA�S�A�Q�A���A��KA���A���A���A���A�-�A�kQA���A��iA�JXA�ӏA�A�2�A��A��Ay\�Ar4�AnɆAi��AdU�AaQA^!�A])_A\�AZ�AXAU5�AR��AQ~AO�zAN��AL��AIƨAFOAAW�A=��A9M�A4��A3�0A2��A1�XA1"hA.�A,y>A,�A+-�A+*0A+��A,��A.$tA.�A.�A,�zA,5?A,�A,�A,�A+�A,A,�A,$A,0UA,0�A+��A)�'A(��A'��A&��A&��A&e�A&YKA&+A&��A'�A'A'�/A'�A%��A'!�A'�A'��A'OvA&C�A%A�A$��A#��A"�eA!eA �DA ѷA A AL0A�jA@A��A�[Ac AaA�9A�HA��A��A�A}VAL�AZA��A��A��A�A~�A/�A��As�A1A�_AR�A�AjA��A�{A�A��AϫA�7A"hA�A�{A�A[WA�EAB[Au�A�ATaAA�A&�A��A��AIRA
=A��A��AU�AH�A�A��A�A�hA\�A2aA�A�'A��A�:AffAA
dZA
*0A	��A	/�A	A�\A��A{JA�A��A�DA^5AB[A4nA�TAX�A	lAzA<6A�AA�tA�A�4A$tAԕA�:Ae�A/�A �|A �fA b�A @�~�@���@�(�@�x�@��X@�9X@�� @��@�O@��@��$@��@�\)@� \@��2@��u@�5?@��@@�U�@�֡@���@�h�@�خ@�}�@�=@�@��@��@���@�f@�z�@��@��@��@�Q@�@�V@��/@�J�@鐗@�!�@譬@艠@�=q@�+k@� �@��.@簊@��@���@��@�?}@���@�"�@��Z@��@�r�@�/�@��a@�@O@��'@���@�j�@�E�@��@۵t@��@��@�_@��@���@غ�@؎�@�YK@׆�@� \@��p@֣@�n�@�J#@�6�@Ӝ@��@�L0@�l�@з�@Ϲ�@�e,@�)_@ά�@��@��&@̓{@̧@��A@˃{@� \@���@�ff@�!@�ƨ@�s�@��,@ȣ@Ȉ�@ȅ�@�|�@�@��,@�
�@�w2@��@�H@�@.@�@�}V@�bN@�X@��z@��a@��t@���@�� @��}@�O@���@�m�@���@�z@��@��@�$�@��d@�U�@��@�ں@�Z@���@�C@���@��@���@���@�C�@�]�@�q@��{@��@�$�@�dZ@�u�@���@�F�@��@�Ov@��.@���@���@�T�@��I@�&�@���@���@���@�f�@�!-@��K@���@�U2@�#:@���@�-w@��@���@��'@���@�%�@��@�F@�+@�Ĝ@�N�@��&@�o�@�4@�S@���@��4@�h�@�H@��W@�x@��@��@���@��Y@�:�@���@���@�o@�Z�@��@�C-@��@���@��=@�!�@��M@��]@���@���@�W�@�.�@�  @��K@��@�+@��@���@��A@�<�@�@��&@�ƨ@�{J@�F�@��@��@�m�@�D�@��w@���@��{@�.I@��@�*�@���@��@��N@��V@�H�@�ȴ@�Xy@���@�33@���@�h
@�2�@��@��7@�f�@�o@��O@�l�@��@��@���@�e,@��@���@��O@�l"@�O@��@�zx@�@O@���@���@��@��@�~(@�6@��@�	@��@���@���@�%F@��b@�/�@���@�v`@�Q�@��@��@�z@�5?@��@��q@�o�@��@���@��'@���@�|�@�;�@��.@��@���@�&@��@���@�<�@��]@��j@��z@�a�@��s@�y>@�C-@��@��@���@���@���@�dZ@�"�@���@���@�?�@�!�@��@���@�N<@�,�@��@��s@��@�z@�d�@�J�@�M@���@�*0@���@��1@��@���@���@�x�@�/@��]@��@�`�@�3�@�W@�@@iD@=@~��@~O@}��@}8�@|��@|e�@{��@{��@{v`@{K�@{!-@z��@z^5@z.�@y��@y�@x��@x��@x�5@xی@x�@w�
@w�	@w+@v��@v��@v�@vq�@vJ@um]@u%@t�@t��@t�u@tu�@tN�@s�@sW?@r�1@rO@q�-@qa�@q@ptT@p(�@p�@o�&@o�V@o��@oC�@ns�@m��@m��@m^�@mIR@m4@mq@l��@l��@k�+@k�V@kE9@k
=@j��@j:*@j�@i�N@h|�@g��@g_p@gF�@f�@f҉@f�'@f�+@f�@e��@d��@d�4@d~(@d$@c�r@c��@c>�@b��@bL0@a�9@a��@`��@`~@_˒@_�@_'�@^�m@^��@^$�@]�j@]c�@]@\�@\�@\�o@\Xy@\>B@[�+@[b�@['�@Z�@Z��@Y��@Y*0@YV@X��@X�E@Xu�@X�@W�V@V��@V��@V�b@VTa@U�@Us�@U�@T��@TZ@T~@S��@S�q@S9�@R�}@Rq�@R1�@Q�M@Q�@P�@P~(@PH@P�@O��@O&@N�]@N��@N��@NQ@M��@M�M@Lی@Lz�@Kݘ@K~�@K6z@J��@Ji�@J�@I�z@Io @H�@H�u@G�a@G�P@G1�@F�]@F�F@FB[@E�z@E�=@Ec�@D�	@DS�@D(�@C��@CMj@C�@Bȴ@B��@B~�@B@�@A��@A^�@@�p@@PH@?��@?dZ@>�R@>~�@>R�@>Q@>Ov@>&�@=�@=F@<�@<�z@<-�@;��@;��@;4�@:�y@:��@:C�@:6�@9�D@9u�@9@8�@8oi@8>B@7�@7�q@7��@7�Q@7��@71�@6�]@6��@6ff@6e@5�'@4�f@4g8@4V�@49X@47@3خ@3��@3t�@3$t@2R�@1��@1��@1��@1J�@0ی@0g8@0'R@/�]@/�K@/s@/>�@/�@.�M@.�m@.q�@.M�@.:*@.O@.u@-�>@-�3@-�~@-8�@,��@,b@+��@+��@+W?@+'�@+o@+S@*�X@*��@*Q@*$�@)��@)�@)Y�@)=�@(�f@(Ĝ@(��@(�D@(tT@(K^@'�@'�@'�0@'~�@'C�@'�@&�c@&��@&i�@&{@&�@%��@%��@%��@%`B@%�@%�@%�@$�@$֡@$�4@$��@$|�@#��@#F�@#
=@"�!@"H�@"&�@"�@!�@!��@!��@!`B@!Dg@!�@ �5@ �@ PH@ *�@�m@�6@�[@�:@qv@Z�@��@z@@�@c�@A @�|@ی@��@V�@ �@�k@l�@9�@��@�B@h
@	@��@��@��@Y�@*0@��@�5@�/@��@�_@D�@�6@�$@P�@$t@�X@�+@C�@0U@�n@2a@��@��@�z@Q�@!@�@J#@�@��@�2@ߤ@�<@�F@c @E�@	@�T@��@w2@j@F@&�@��@��@r�@C-@  @�W@�a@�@�F@��@C@�<@�b@h
@C�@+k@��@��@o @B�@��@��@|�@  @��@a@1�@(@
��@
��@
p;@
_�@
Ta@
Ov@
=q@
�@	ϫ@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB�8B�8B�mB�LB�fB�2B�2B��B�8B�mB��B�B�*B�0B�B�B�sB�$B��B�]B��B��B�xB�AB�7B�B	KB
�TB
��B
��B
�AB
�\B
��B
/ B
6zB
?�B
@4B
<6B
2GB
"�B
,B
B	�B	�B	�5B	��B	�;B	��B	|B	d�B	S�B	I�B	A�B	>�B	=�B	9�B	5tB	2�B	5tB	7�B	<B	=VB	B'B	R�B	P}B	>BB	+�B	$�B	�B	gB	�B�qB��B�B� B�!B�B	�B	'B	J�B	o�B	��B	�sB	��B	�
B	�BB	��B	��B	�B	�B	յB	��B	�6B	�!B	�|B	��B	�fB	�B	�B	�'B	�RB	��B
{B
�B
$�B
(�B
9�B
BB
=�B
\)B
i�B
k�B
k�B
g�B
d&B
i�B
h�B
b�B
Y�B
YeB
bhB
bhB
_B
a|B
n�B
pUB
p�B
pUB
q'B
p!B
lB
m�B
o B
j�B
ffB
m�B
q�B
m�B
hXB
h�B
h�B
h$B
g8B
h
B
g�B
gmB
e�B
d�B
e,B
d�B
k�B
m�B
h�B
j�B
nB
mwB
kQB
kB
j0B
i�B
g8B
d�B
cnB
`�B
Z�B
YB
Z�B
^B
_;B
^B
^�B
`\B
`BB
`'B
_�B
_�B
_;B
^�B
^B
]~B
]IB
\xB
\CB
[WB
Z�B
Z�B
ZB
Y1B
U�B
U2B
S�B
PbB
PHB
O(B
J�B
I�B
H�B
G_B
F�B
E�B
D�B
C�B
DB
CaB
C{B
BB
@�B
?B
=qB
9�B
7�B
7�B
5%B
49B
3�B
3hB
2�B
2|B
1�B
1�B
0�B
0B
/OB
.�B
.IB
-�B
,�B
+�B
*KB
)�B
)DB
(�B
'�B
'mB
'B
&�B
&fB
%�B
%�B
%zB
%B
$�B
$ZB
#TB
"�B
!�B
B
~B
�B
yB
mB
mB
_B
B
B
MB
FB
B
&B
B
�B
�B
�B
�B
{B
{B
�B
�B
�B
B
�B
MB
�B
�B
�B
�B
:B
hB
NB
�B
�B
}B
�B
�B
(B
(B
vB
�B
B
(B
�B
pB
pB
<B
�B
�B
�B
�B
�B
B
�B
\B
"B
JB

�B
DB
^B
B
B

�B

	B
	�B
	�B
	RB

rB

=B

�B
�B
0B
0B
DB
0B
�B
"B
<B
<B
�B
"B
�B
6B
�B
6B
�B

#B
	lB
	�B

#B
	�B
B
�B
�B
zB

�B
^B
�B
 B
:B
B
gB
B
�B
 B
bB
.B
�B
BB
�B
<B
�B
PB
PB
B
B
�B
^B
�B
BB
�B
aB
B
"B
<B
�B
	B
EB
EB
�B
1B
B
�B
�B
�B
%B
_B
	7B

#B

�B

�B

rB

=B

	B

	B
	�B
	�B
B
�B
�B
�B
VB
"B
B
�B
�B
\B
�B
hB
�B
�B
,B
mB
�B
�B
�B
�B
FB
gB
9B
SB
�B
9B
{B
�B
�B
B
�B
�B
�B
1B
KB
KB
B
�B
�B
�B
B
7B
7B
�B
�B
	B
�B
	B
	B
	B
�B
=B
=B
CB
]B
]B
�B
/B
�B
�B
�B
�B
�B
/B
�B
�B
~B
B
OB
�B
�B
VB
�B
�B
�B
 �B
 �B
 �B
 �B
!bB
"NB
#B
"�B
# B
#TB
#TB
#�B
$B
$ZB
$tB
$�B
$�B
$�B
%�B
%�B
%�B
&fB
'mB
'�B
'�B
'�B
'�B
'�B
'�B
(sB
)B
)�B
)�B
(XB
(�B
)yB
)B
(�B
(
B
(�B
)�B
*�B
,"B
-]B
.�B
.�B
/iB
0!B
0�B
0�B
0�B
1�B
2B
2-B
3B
33B
33B
3B
3�B
4B
4B
49B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
6`B
6zB
6�B
7�B
7�B
7�B
8B
88B
8lB
8�B
8lB
8RB
8lB
8�B
9rB
9XB
9�B
:^B
:�B
:�B
:�B
;0B
;�B
;�B
<B
<6B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>B
>]B
>�B
>�B
>�B
>�B
?B
?.B
?.B
?HB
@ B
@B
@B
@B
@ B
@iB
@�B
AB
A;B
AUB
AUB
AoB
A�B
A�B
B'B
BuB
BuB
B�B
B�B
B�B
B�B
C-B
C�B
C�B
D3B
D�B
D�B
EB
E�B
E�B
E�B
E�B
FB
E�B
E�B
E�B
E�B
F%B
FtB
FtB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G_B
G+B
G+B
H1B
IB
IlB
IlB
I�B
I�B
I�B
I�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
LB
K�B
LB
L�B
MB
MB
M�B
N<B
NpB
NVB
N�B
N�B
OB
OBB
OvB
O�B
O�B
O�B
P.B
P.B
P.B
P.B
PbB
P�B
P�B
P�B
Q4B
Q�B
RB
RB
RB
RB
R:B
RoB
R�B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
UgB
UgB
U�B
U�B
U�B
V9B
V�B
V�B
V�B
WYB
W�B
W�B
W�B
XB
X+B
X�B
X�B
YB
YKB
YKB
YeB
Y�B
Y�B
ZkB
ZkB
Z�B
[=B
[WB
[WB
\B
\]B
\xB
\�B
]B
]IB
^B
^B
^OB
^�B
^�B
_B
_VB
_VB
_pB
_�B
`'B
`B
`�B
`�B
`�B
aB
a-B
abB
a�B
b4B
b�B
cnB
c�B
d&B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
gB
g�B
g�B
g�B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
j0B
jB
jB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
kB
kB
kB
kB
kkB
k�B
k�B
k�B
k�B
lWB
lqB
l�B
l�B
l�B
mB
mB
mCB
mCB
m]B
m�B
m�B
m�B
nB
nB
nB
nB
ncB
n�B
oB
o�B
o�B
o�B
o�B
p!B
p!B
p!B
poB
pUB
p�B
p�B
p�B
p�B
q'B
q'B
qvB
q�B
q�B
q�B
q�B
rB
r�B
r�B
r�B
s3B
shB
s�B
shB
s�B
t9B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
vB
u�B
v+B
vFB
v+B
wB
wLB
w�B
w�B
xB
xB
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
yrB
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
z�B
z�B
{dB
{JB
{B
{0B
{�B
{�B
{�B
{B
{�B
|B
|B
|PB
|6B
|�B
}B
}qB
}�B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�OB
��B
�OB
�B
�;B
��B
��B
��B
�'B
�[B
�uB
�GB
��B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
�9B
�SB
�mB
�mB
��B
��B
�?B
�YB
�tB
��B
��B
��B
��B
��B
�B
��B
�B
�B
�fB
��B
��B
��B
�B
�7B
�RB
��B
��B
�	B
��B
�)B
�DB
�xB
�xB
��B
�B
�JB
�JB
�JB
�JB
�JB
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�!B�;B��B��B��B��B��B�B��B�B�B��B��B�!B�B�!B��B�pB�pB��B��B��B�jB�jB��B�pB��B�'B�HB�bB�hB��B�NB��B�vB��B��B��B�-B�hB��B�RB��B	MB
�HB
��B
�vB
�FB
�.B
ѝB
6zB
6�B
@B
=VB
;0B
-�B
�B
�B
B	�CB	�tB	�)B	��B	��B	�VB	{�B	b�B	OvB	ESB	;B	8�B	8lB	4�B	1B	./B	/�B	1�B	6+B	8lB	>�B	O�B	NpB	;B	)*B	!�B	uB��B�B�B�B�B��B�B�B�B	�B	A�B	g�B	{�B	��B	��B	�vB	�yB	��B	��B	�	B	�B	͹B	��B	�B	�_B	�B	�nB	߾B	�pB	�B	�_B	�oB	��B	��B
�B
�B
 �B
2�B
:�B
4�B
S�B
a�B
d�B
e`B
`�B
\�B
c:B
bNB
\]B
RoB
Q�B
[=B
[qB
WsB
YB
f�B
h�B
iDB
h�B
i�B
iB
dZB
e�B
g�B
cnB
^�B
e�B
j�B
f�B
`�B
aB
aHB
`�B
_�B
`�B
`�B
_�B
^B
]~B
^B
]~B
d&B
f�B
`�B
b�B
f�B
e�B
c�B
c�B
b�B
b�B
`B
]�B
\xB
Y�B
S@B
Q�B
R�B
VmB
W�B
VmB
W?B
X�B
X�B
XyB
W�B
W�B
W�B
W
B
V9B
U�B
U�B
T�B
T�B
S�B
S&B
R�B
R�B
Q�B
N<B
M�B
LJB
H�B
IB
G�B
C-B
B'B
A;B
?�B
>�B
=�B
<�B
<jB
<�B
;�B
<6B
:xB
9$B
7�B
6FB
2|B
0oB
/�B
-�B
,�B
,"B
+�B
+6B
*�B
*B
*KB
)_B
(�B
'�B
'mB
&�B
&B
%zB
$ZB
"�B
"�B
!�B
!B
 'B
�B
pB
!B
�B
B
B
�B
IB
IB
�B
�B
=B
7B
�B
B
:B
B
�B
�B
�B
�B
�B
�B
�B
~B
�B
	lB
1B
	�B
�B
�B
�B
�B
<B
vB
B
VB
B
�B
B
�B
xB
^B

�B
	�B
	�B
	lB
	lB
	B
�B
B
�B
�B
�B
_B
�B
zB
�B
�B
�B
�B
_B
�B
EB
�B

�B
	�B
	RB
�B
�B
B
{B
�B
�B
aB
�B
GB
[B
AB
AB
�B
�B
�B
-B
MB
�B
�B
�B
�B
�B
?B
tB
�B
�B
�B
%B
�B
mB
�B
tB
�B
�B
�B
�B
�B
 �B	�}B	�B	��B
aB
aB
�B

rB

�B

#B
�B
�B
�B
	�B
�B
�B
KB
�B
_B
�B
YB
�B
�B
mB
�B
MB
{B
�B
_B
jB
B
	�B
tB
B
mB
oB	��B	��B	��B
 �B
 �B
 4B
 B	��B	�wB	��B
�B
uB
�B
�B
�B
�B
�B
[B
�B
B
aB
�B
B
+B
�B
tB
tB
EB
_B
�B
KB
	�B
DB
�B
~B
�B
�B
B
B
B
�B
�B
�B
�B
(B
�B
B
�B
B
VB
 B
4B
hB
hB
hB
�B
�B
�B
 B
:B
TB
�B
�B
B
B
[B
@B
[B
@B
@B
[B
uB
�B
�B
�B
�B
�B
gB
B
B
gB
B
B
gB
�B
B
�B
�B
�B
?B
?B
�B
�B
+B
EB
�B
�B
1B
eB
�B
�B
=B
=B
qB
�B
�B
B
CB
�B
�B
�B
B
/B
�B
B
5B
�B
�B
�B
�B
�B
�B
 B
�B
 �B
!|B
"NB
!�B
 �B
!B
!�B
!bB
!B
 \B
!-B
"4B
# B
$ZB
%�B
&�B
'8B
'�B
(XB
(�B
)DB
)_B
)�B
*eB
*�B
+kB
+kB
+�B
+kB
,B
,WB
,qB
,qB
,�B
,�B
,�B
,�B
-CB
-�B
-�B
-�B
./B
.�B
.�B
/OB
/�B
/�B
0!B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
2-B
2�B
2�B
3B
33B
3�B
4B
4B
4nB
4�B
4�B
4�B
4�B
5B
5�B
5�B
6B
6B
6FB
6`B
6�B
6�B
6�B
7B
72B
7LB
7�B
7�B
7�B
88B
88B
88B
8lB
88B
8�B
8�B
9>B
9rB
9�B
9�B
9�B
9�B
:B
:^B
:�B
:�B
:�B
:�B
;B
;0B
;B
<B
<PB
<�B
<�B
=B
=VB
=�B
>B
>B
>B
>BB
>(B
>(B
>B
>(B
>]B
>�B
>�B
>�B
>�B
>�B
?B
?HB
?�B
@4B
@B
?�B
?�B
?}B
?�B
@�B
A;B
A�B
A�B
BB
A�B
A�B
BB
C-B
C{B
C�B
C�B
C�B
DB
D3B
DMB
DMB
DMB
D�B
EmB
E�B
F%B
FtB
F�B
F�B
F�B
GB
GEB
GzB
G�B
G�B
G�B
HB
HfB
HfB
H�B
HfB
H�B
IB
IB
I7B
I�B
J#B
J=B
J=B
J=B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
LJB
L�B
MB
MPB
M�B
M�B
M�B
NB
N"B
N�B
N�B
N�B
OB
O�B
O�B
O�B
P.B
PHB
P}B
P�B
Q4B
QNB
Q�B
Q�B
Q�B
R B
R:B
R�B
R�B
S&B
S[B
S�B
S�B
TaB
T�B
T�B
T�B
UgB
U�B
V9B
VB
V�B
V�B
V�B
W?B
W�B
W�B
W�B
XB
X_B
X_B
X�B
X�B
X�B
YKB
YKB
Y�B
ZB
Z�B
[	B
[�B
\)B
\]B
]B
]�B
^B
^B
^B
^B
^B
^�B
^�B
_!B
_pB
_�B
_�B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
`�B
bB
bhB
b�B
b�B
b�B
b�B
c:B
c B
c B
c B
cB
cB
c B
c B
c:B
c:B
c:B
cnB
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
d�B
eB
eFB
eFB
ezB
ezB
e�B
e�B
e�B
f2B
fLB
f2B
fLB
ffB
f�B
f�B
gmB
g�B
g�B
g�B
h$B
hXB
hXB
hXB
h�B
h�B
h�B
h�B
h�B
i*B
iDB
i_B
i�B
i�B
i�B
jB
jB
jKB
j�B
kB
k6B
kkB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
mB
m)B
m�B
nB
m�B
nB
nIB
n/B
ncB
n}B
n}B
oiB
o�B
o�B
o�B
p;B
p!B
p;B
poB
p�B
p�B
p�B
p�B
p�B
qB
qB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r�B
s3B
s�B
s�B
sMB
shB
s�B
tB
tB
s�B
tB
tTB
t9B
t�B
t�B
uB
u?B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
xB
xB
xRB
x�B
x�B
x�B
yXB
yXB
y�B
y�B
y�B
z^B
z�B
z�B
{B
{�B
{�B
{�B
{�B
|B
|6B
|PB
|�B
|�B
}B
}qB
}qB
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
~�B
B
B
.B
cB
�B
�OB
�OB
��B
��B
��B
��B
�;B
�oB
��B
��B
�AB
�[B
�B
�{B
�{B
��B
��B
��B
�MB
�gB
�MB
�gB
��B
��B
��B
�B
�933111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<S��<#�
<��<y?�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9998(+-0.0000), deepest deltaS=-0.008(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-1500(dbar) is excluded in mapping;                                                                                                                                       Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202101260054212021012600542120210126005421202101260200492021012602004920210126020049202207271541112022072715411120220727154111  JA  ARFMdecpA30a                                                                20210114184009  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210114184025  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210114184026  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210114184026  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210114184026  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210114184027  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210114184027                      G�O�G�O�G�O�                JA  ARUP                                                                        20210114185228                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20210116000000  CF  PSAL_ADJUSTED_QC@z�@�  G�O�                JM  ARCAJMQC2.0                                                                 20210125155421  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210125155421  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20210125170049  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727064111  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091506                      G�O�G�O�G�O�                