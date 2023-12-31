CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-18T09:38:04Z creation;2020-05-18T09:38:06Z conversion to V3.1;2022-08-02T05:11:00Z update;     
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
_FillValue                 �  ]P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20200518093804  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               /A   JA  A30_8420_047                    2C  D   APEX                            8420                            2.11.2                          846 @�K�Z�1   @�K`� @/1�i�B��b�_o� 1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�33@�  A��A   A@  A`  A�  A���A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`��Bg33Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B虚B�  B���B���B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>33C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@
=@��\@���A�A ��A@(�A`��A�
A��\A��A�=qA�  A�  A�(�A�Q�B (�B�B{B
=B �B(�B0{B8
=B@  BH  BP�BX\)B`��BgffBo�HBw�HB��B�  B�  B���B�  B�
=B�B�  B�B�(�B�B�B�  B�
=B���B�
=B�\B�  B���B���B�  B�
=B�
=B�  B�B�
=B��B�RB���B��
B��fB�\B�{C �CC\C�C�C
�C�C�CCCC�CC  C�C�C �C!�qC$  C&  C(�C*
=C,
=C.�C/�qC2  C4
=C6�C8
=C:C<{C>33C?�CB�CD�CF�CH�CJ�CL
=CN�CPCR�CT
=CVCX�CZC\�C^
=C`�Cb�Cd�Ce�qCh�Cj�Cl  Cn�Cp\Cr)Ct�Cu�qCw�qCy��C|  C~C�HC���C���C���C�HC��C�  C��C�fC��C��C��C�C�fC��C�  C�HC���C�HC��C�HC�  C��C�fC�  C��qC���C��C��C�HC��C��C�C��C�HC���C��C�
=C�C��C�C�C��C��C���C�  C��C�C��C��C�C�C�C��C�HC�C��C�C�C�fC��C�  C��C��C�HC�HC�HC��C�HC�  C�HC�HC��C�C��C��C�  C�  C�HC�  C�  C�  C��C��C��C�  C��)C�  C�HC�  C��C�C��C��C��C�fC��C��qC��)C��)C��qC�  C�C��C��C��C�HC���C���C���C��C��C�fC�HC���C�HC�C��C�HC�HC�HC��C��C��C��C��C��C��D �D ��D�D��DHD��D��D� D�D� D �D�HD�\D��D �D��DHD�3D	�D	��D
{D
�3D �D��D �D\DHD��DHD� D��D\D  D��D3D��D��D�HD �D~�D �D�HD �D�HD �D� D  D\D  D\D��D��DHD��DHD�HD�D\D �D�3D�D��D  D�HD �D ��D!3D!��D!��D"~�D#  D#�HD$�D$��D$��D%� D&�D&�3D'�D'��D'�\D(~�D)HD)��D)�\D*�3D+�D+��D,3D,�3D-�D-��D-�D.\D/  D/� D0  D0� D1�D1��D2�D2��D3  D3�HD4�D4�HD5�D5��D6 �D6��D6�\D7��D8�D8��D9 �D9�HD:�D:��D;HD;��D;�\D<\D<�\D=� D> �D>\D>��D?� D@�D@��DAHDA� DA��DB\DC  DC�HDD3DD�3DEDE� DF �DF�HDGHDG� DH �DH��DI  DI�HDJ�DJ�HDK �DK��DL �DL��DM�DM��DN�DN��DO  DO� DO�\DP� DQHDQ�3DR{DR��DS  DS\DT �DT��DU�DU��DU�\DV\DW  DW��DX�DX� DYHDY��DZHDZ�HD[�D[��D\�D\��D]�D]��D^  D^�HD_�D_�HD`HD`��Da�Da��Db �Db� Dc3Dc�3Dd�Dd�HDe �De�HDf�Df��Dg�Dg��Dh�Dh��Di�Di�3Dj3Dj��Dk�Dk��Dl�Dl��Dl�\Dm��Dn�Dn��Do  Do��Dp�Dp��Dq3Dq��Dr�Dr��Ds�Ds�HDt �Dt��Du�Du��Dv �Dv� Dv��Dw��Dx{Dx��Dy3Dy��Dz �Dz\D{ �D{��D| �D|�3D}�D}��D~�D~��DHD��D� �D�@�D���D��RD� RD�@�D���D���D� RD�@�D���D��HD� RD�?�D��D�� D�HD�AHD��HD��HD�  D�@RD��HD��RD� �D�B=D���D���D��D�AHD���D��HD� �D�@�D��HD��RD�  D�@RD�� D���D� �D�@�D���D��HD� RD�@ D��D�� D� �D�A�D���D���D� RD�@ D�� D��RD� �D�AHD���D��HD�HD�?�D�� D��RD� RD�@�D�\D��
D�  D�@�D���D���D� RD�?�D���D��HD� �D�?�D�� D��RD� RD�@RD��RD���D� �D�A�D��HD���D� RD�?
D���D���D� �D�AHD��RD�� D� �D�@ D��D���D��D�AHD���D��RD���D�@ D���D��RD� RD�@�D��RD���D�HD�@RD���D���D��D�@�D��HD��HD� �D�@�D��RD��RD� �D�@�D��HD���D� �D�@RD�\D���D��D�@�D���D���D��D�@�D���D���D� RD�@�D�� D���D�  D�?�D�� D���D� �D�@�D���D�� D� �D�?�D��D���D�  D�?�D�\D���D� RD�AHD��HD���D� RD�?\D��RD���D� �D�@�D���D��\D�  D�@�D�� D���D��D�@�D��RD�� D���D�?�D�\D���D� RD�@RD�
D��
D���D�@ D��RD���D��D�@�D��D��RD�HD�A�D���D��\D�  D�@�D���D���D�HD�A�D��RD���D� RD�B=D��=D��=D��D�@�D�� D�� D� RD�?�D��D�� D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��RD�HD�@RD��D�� D� �D�A�D���D���D� �D�@ D��D��RD� RD�@ D��RD��HD� �D�@ D��RD��HD� �D�@�D���D��RD� RD�@�D���D���D� �D�A�D���D���D�HD�@�D D���D��D�A�DÀ�D�� D��\D�?�DĀ�D���D� �D�A�DŁ�D���D� �D�AHDƀ Dƿ�D� �D�@ Dǀ D���D��D�B=DȀ D��RD� �D�@ Dɀ D���D� �D�@�Dʀ�D���D�  D�@ Dˀ�D�� D���D�@ D��D��RD� RD�@�D́�D���D� RD�@�D΀RD��RD�HD�B=Dρ�D���D�HD�@�DЀ Dп�D� �D�A�Dс�D���D� RD�@RDҀ�D��HD��D�@RD��D���D� RD�@�DԁHD���D�HD�@�D�\D�� D�  D�@ Dր�Dֿ�D���D�@�Dׁ�D���D��D�@�D��D���D� �D�@�DفHD�� D��
D�@ Dڀ�D���D�HD�A�Dہ�D��HD��D�AHD܀�D��RD� �D�@�D݀�D��HD� �D�@ DހRD���D� �D�@�D߀RD���D� �D�@ D���D�� D�  D�@�D��D���D� �D�AHD⁚D���D�  D�@ D� D�\D��\D�?�D䀤D��HD�HD�@RD��D���D� �D�@�D�RD���D� �D�@�D瀤D��HD� �D�AHD��D���D��D�@�D逤D�� D�  D�@�DꁚD��HD� RD�@�D끚D��HD�HD�A�D쀤D쿮D� �D�@�D��D��HD� RD�AHD��D�\D�  D�@RD��D���D� �D�@ D���D���D� �D�?�D�
D�
D��
D�@�D�=D���D���D�@�D�RD�\D�  D�@ D� D���D��\D�AHD���D��RD� �D�B=D��HD�� D� �D�A�D���D�� D��
D�@�D��=D���D��D�@ D��RD��=D�  D�@RD��=D���D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�k�A�h�A�h�A�gmA�e�A�l�A�o�A�v�A�w2A�y	A�2aA�
=A��XAʵtAʮ�AʭCAʥ�Aʡ�Aʝ~Aʒ:AʉlA�D3A�RTA���A�K�A�PA�@�A��A���A�	A�3�A�Q�A�H�A��#A�v�A��A���A��A��(A�_�A�?�A��]A���A���A�x�A��A�jA�E9A�ݘA��A��A�k�A��JA��A�_�A��:A��A��9A��
A�^A���A�H�A�@A��A���A�V9A���A���A���A�A��pA�+A��oA��A�\�A��A��=A��OA��A�CaA�&�A�@OA�)�A�P}A���A��RA�aA�(�A��=A��JA�S�A��A�e�A}�A|��A{u�Ar��Al��Ai��Af�sAdk�Aa�A^5�AZ;AV��AR�&AP��AL��AIJAG��AD� ABTaA@��A?�FA>
=A:A8#�A6�{A3�IA2GA1R�A/��A.{JA,��A,o�A+2aA*GA)|�A)YKA( �A&�NA%&�A#��A#�A!�A ��A  iAf�A�\AxlA�\A�A�A��Al"A�bA��A�eA6�A	A�A�<A��A��AFA�\A�{A��A��A�AMA�AU�A�)Ae�A��Ah�AX�A��A��AD�A��A�KA�qA��A�{A�A�:A�A�A�XA6A
ȴA
xlA
S�A	�rA	l�A	;�A	�A�3AL0A��AdZA1'A��A�A	A��AB�Aw�A�.AY�AMA��A�yA^�A�AhsAYA �9A ƨA �FA A�@���@��8@���@�*�@�P�@�q@�=q@��7@���@� �@�RT@�a|@��}@��@�6�@��Z@�_@��6@���@�S�@�%@��1@�	@�"@�>�@��@�x@�c @�R�@��Z@�/@�q@@��v@��@�p�@��@�j@�_@�	@�@��@��@�!@���@�x@���@�kQ@��]@�}�@�D�@��@�[W@�j@�� @�IR@��@�1@�	@ථ@ߧ�@���@�m�@�'R@��>@݅�@��@ܬ@��@�;@�h�@���@ح�@ׯ�@׊	@�>�@��@�y>@�b@Փ�@�(@Ԋr@��@�s�@��@�u�@��9@�0�@е@�u%@��.@ψf@�@O@�ѷ@�.�@͗$@�7L@��@�͟@̾�@̞�@�y>@���@ˀ4@�.I@ʽ<@�e@ɲ�@Ɏ"@�"�@ȵ@�6�@���@�m]@�;d@��@ƞ@�]d@�7@�˒@�w2@��c@İ�@�2�@÷@�S&@�!-@°�@�B[@��j@���@���@�=�@�kQ@��@���@���@�7�@���@���@�X@��f@���@���@�Z@���@��0@�IR@���@�b@��'@�c@�*0@���@���@�1'@��r@��X@�|@�+�@���@�$�@�o @��]@�M@���@�\�@��@���@�:*@��d@��@�0�@���@�Ov@�ϫ@��M@�F�@��@���@���@�8�@�x@��@���@�V@�/�@��@���@��"@�\)@��@�֡@��@��@�rG@�@O@��,@�M@�=@��@���@�[�@��@��@�F�@�%@���@�0U@���@���@��@��)@���@�{�@�0U@��W@��F@�s�@�L�@��@���@���@�|�@�Z�@�1�@�4@��
@��@�]�@�8�@��@�J�@��@���@�W?@���@�ѷ@��6@�u�@�$�@��@��@�m]@���@��r@�>B@�1'@��@��@��^@�]�@���@�h
@�!�@�r�@��Y@�K^@�{@���@�8�@���@���@�@��@��@��@��r@�O@���@�*�@�@�@�q�@���@���@��@��j@���@���@�b�@��@��e@�u%@�_@�}V@�u%@�W�@��@�˒@��h@�o @�RT@�o@��1@�H�@���@�v`@�T�@�!�@��@���@�~@���@�w2@��@��@�p;@�2�@��@���@��9@��[@��@�^�@��@�E�@�L0@�A�@�M@���@���@�m]@�J�@��@�Ɇ@�h
@�$@��@���@���@�P�@�<6@��@���@���@���@�W�@�O@��Z@��^@�dZ@���@��H@��6@���@���@���@��@���@�C-@���@��$@�|@�e,@��@���@��m@���@���@���@���@�h�@���@��@�/@��K@��@���@��h@�z�@�M@�A�@�!@��@�$@iD@J#@~�A@}��@}-w@|�j@|��@|7�@{�
@{a@z�@zi�@y�)@y��@xU2@w��@w�K@w�k@w(@v�B@vxl@u��@u�7@uJ�@t�z@t7�@s�6@s��@s.I@r�H@r~�@r$�@q�9@q:�@p��@p:�@o��@oqv@n�B@nZ�@m��@m�@l�@k�@kP�@j��@j}V@j6�@j@i�@if�@i/@h�`@h��@h*�@gƨ@ga@f�c@f@eu�@e�@e@d�@d`�@d:�@d7@c��@cb�@b�m@b&�@a�9@a�@a@`ی@`�O@` �@_��@_]�@_�@^�B@^�m@^�R@^��@]�@]��@]��@]8�@\�	@\j@[��@[��@[X�@[@O@Z��@Y��@Y^�@Y \@X�5@XbN@W��@W��@WW?@W'�@V�,@V\�@U�^@U�@Tj@T�@S�F@S]�@R��@R0U@R�@Q�@Q��@Qu�@Q;@P�@P��@Pz�@PFt@O��@O��@O�{@O/�@N��@N{�@N�@M�9@M�X@M�~@M\�@MB�@M8�@L��@L'R@K��@KMj@J�@Jz@I�@Ia�@I@@Hu�@HD�@G��@G��@Gx@G�@F��@Fc @E�h@D�?@D��@D_@C��@C��@C�@B��@Bs�@BL0@B�@A��@Aa�@AJ�@A0�@A�@@�_@?�&@?��@?�@?��@?�{@?y�@?S�@>�@>&�@=@=�h@=�@<�I@<tT@;�*@:�2@:ȴ@:Z�@:GE@9�@9��@8��@8�u@8�I@8tT@87�@8x@7�0@7|�@71�@6��@5��@5@4�5@4�@4tT@4�@3�w@3��@3H�@2ߤ@2��@2�B@2�\@2.�@1�.@1�9@1��@18�@1@0�Y@0-�@/�r@/�@/qv@/�@.��@.B[@-�D@-�3@-�@-;@,Ɇ@,7�@+�Q@+X�@*�,@*��@*�\@*h
@*�@*_@)��@)�j@)Vm@)B�@)A @)A @)4@)�@(��@(��@(bN@(-�@(1@'خ@'e�@'�@&��@&�\@&3�@%�T@%��@%�~@%4@$�p@$~(@$]d@$�@#��@#{J@#j�@#U�@#A�@"��@"��@!�>@!��@!��@!Y�@ �@ �@ �@ _@ 6@ �@�@�[@��@��@1�@�@�c@�@�s@�+@�@�^@��@s�@N<@8�@+�@�P@�/@�j@�_@tT@c�@M@W?@��@h
@\�@8�@e@��@�X@��@x�@a�@�`@��@PH@@� @��@��@��@o�@RT@!-@
=@�@�]@�!@��@q�@R�@	@�)@�N@�@��@Q�@��@��@�.@r�@A�@�+@�4@$t@S@�@@�2@�<@z@M�@#:@�@{@�@�@u@�Z@�#@��@��@X@�f@�?@��@��@�4@r�@(�@�A@��@�a@�@�@�@�$@��@��@n/@o�@s@\)@�@o@�M@��@��@n�@c @0U@�-@�7@m]@e,@?}@�@�D@e�@Q�@C-@A�@C-@Ft@Ft@(�@~�@C�@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�k�A�h�A�h�A�gmA�e�A�l�A�o�A�v�A�w2A�y	A�2aA�
=A��XAʵtAʮ�AʭCAʥ�Aʡ�Aʝ~Aʒ:AʉlA�D3A�RTA���A�K�A�PA�@�A��A���A�	A�3�A�Q�A�H�A��#A�v�A��A���A��A��(A�_�A�?�A��]A���A���A�x�A��A�jA�E9A�ݘA��A��A�k�A��JA��A�_�A��:A��A��9A��
A�^A���A�H�A�@A��A���A�V9A���A���A���A�A��pA�+A��oA��A�\�A��A��=A��OA��A�CaA�&�A�@OA�)�A�P}A���A��RA�aA�(�A��=A��JA�S�A��A�e�A}�A|��A{u�Ar��Al��Ai��Af�sAdk�Aa�A^5�AZ;AV��AR�&AP��AL��AIJAG��AD� ABTaA@��A?�FA>
=A:A8#�A6�{A3�IA2GA1R�A/��A.{JA,��A,o�A+2aA*GA)|�A)YKA( �A&�NA%&�A#��A#�A!�A ��A  iAf�A�\AxlA�\A�A�A��Al"A�bA��A�eA6�A	A�A�<A��A��AFA�\A�{A��A��A�AMA�AU�A�)Ae�A��Ah�AX�A��A��AD�A��A�KA�qA��A�{A�A�:A�A�A�XA6A
ȴA
xlA
S�A	�rA	l�A	;�A	�A�3AL0A��AdZA1'A��A�A	A��AB�Aw�A�.AY�AMA��A�yA^�A�AhsAYA �9A ƨA �FA A�@���@��8@���@�*�@�P�@�q@�=q@��7@���@� �@�RT@�a|@��}@��@�6�@��Z@�_@��6@���@�S�@�%@��1@�	@�"@�>�@��@�x@�c @�R�@��Z@�/@�q@@��v@��@�p�@��@�j@�_@�	@�@��@��@�!@���@�x@���@�kQ@��]@�}�@�D�@��@�[W@�j@�� @�IR@��@�1@�	@ථ@ߧ�@���@�m�@�'R@��>@݅�@��@ܬ@��@�;@�h�@���@ح�@ׯ�@׊	@�>�@��@�y>@�b@Փ�@�(@Ԋr@��@�s�@��@�u�@��9@�0�@е@�u%@��.@ψf@�@O@�ѷ@�.�@͗$@�7L@��@�͟@̾�@̞�@�y>@���@ˀ4@�.I@ʽ<@�e@ɲ�@Ɏ"@�"�@ȵ@�6�@���@�m]@�;d@��@ƞ@�]d@�7@�˒@�w2@��c@İ�@�2�@÷@�S&@�!-@°�@�B[@��j@���@���@�=�@�kQ@��@���@���@�7�@���@���@�X@��f@���@���@�Z@���@��0@�IR@���@�b@��'@�c@�*0@���@���@�1'@��r@��X@�|@�+�@���@�$�@�o @��]@�M@���@�\�@��@���@�:*@��d@��@�0�@���@�Ov@�ϫ@��M@�F�@��@���@���@�8�@�x@��@���@�V@�/�@��@���@��"@�\)@��@�֡@��@��@�rG@�@O@��,@�M@�=@��@���@�[�@��@��@�F�@�%@���@�0U@���@���@��@��)@���@�{�@�0U@��W@��F@�s�@�L�@��@���@���@�|�@�Z�@�1�@�4@��
@��@�]�@�8�@��@�J�@��@���@�W?@���@�ѷ@��6@�u�@�$�@��@��@�m]@���@��r@�>B@�1'@��@��@��^@�]�@���@�h
@�!�@�r�@��Y@�K^@�{@���@�8�@���@���@�@��@��@��@��r@�O@���@�*�@�@�@�q�@���@���@��@��j@���@���@�b�@��@��e@�u%@�_@�}V@�u%@�W�@��@�˒@��h@�o @�RT@�o@��1@�H�@���@�v`@�T�@�!�@��@���@�~@���@�w2@��@��@�p;@�2�@��@���@��9@��[@��@�^�@��@�E�@�L0@�A�@�M@���@���@�m]@�J�@��@�Ɇ@�h
@�$@��@���@���@�P�@�<6@��@���@���@���@�W�@�O@��Z@��^@�dZ@���@��H@��6@���@���@���@��@���@�C-@���@��$@�|@�e,@��@���@��m@���@���@���@���@�h�@���@��@�/@��K@��@���@��h@�z�@�M@�A�@�!@��@�$@iD@J#@~�A@}��@}-w@|�j@|��@|7�@{�
@{a@z�@zi�@y�)@y��@xU2@w��@w�K@w�k@w(@v�B@vxl@u��@u�7@uJ�@t�z@t7�@s�6@s��@s.I@r�H@r~�@r$�@q�9@q:�@p��@p:�@o��@oqv@n�B@nZ�@m��@m�@l�@k�@kP�@j��@j}V@j6�@j@i�@if�@i/@h�`@h��@h*�@gƨ@ga@f�c@f@eu�@e�@e@d�@d`�@d:�@d7@c��@cb�@b�m@b&�@a�9@a�@a@`ی@`�O@` �@_��@_]�@_�@^�B@^�m@^�R@^��@]�@]��@]��@]8�@\�	@\j@[��@[��@[X�@[@O@Z��@Y��@Y^�@Y \@X�5@XbN@W��@W��@WW?@W'�@V�,@V\�@U�^@U�@Tj@T�@S�F@S]�@R��@R0U@R�@Q�@Q��@Qu�@Q;@P�@P��@Pz�@PFt@O��@O��@O�{@O/�@N��@N{�@N�@M�9@M�X@M�~@M\�@MB�@M8�@L��@L'R@K��@KMj@J�@Jz@I�@Ia�@I@@Hu�@HD�@G��@G��@Gx@G�@F��@Fc @E�h@D�?@D��@D_@C��@C��@C�@B��@Bs�@BL0@B�@A��@Aa�@AJ�@A0�@A�@@�_@?�&@?��@?�@?��@?�{@?y�@?S�@>�@>&�@=@=�h@=�@<�I@<tT@;�*@:�2@:ȴ@:Z�@:GE@9�@9��@8��@8�u@8�I@8tT@87�@8x@7�0@7|�@71�@6��@5��@5@4�5@4�@4tT@4�@3�w@3��@3H�@2ߤ@2��@2�B@2�\@2.�@1�.@1�9@1��@18�@1@0�Y@0-�@/�r@/�@/qv@/�@.��@.B[@-�D@-�3@-�@-;@,Ɇ@,7�@+�Q@+X�@*�,@*��@*�\@*h
@*�@*_@)��@)�j@)Vm@)B�@)A @)A @)4@)�@(��@(��@(bN@(-�@(1@'خ@'e�@'�@&��@&�\@&3�@%�T@%��@%�~@%4@$�p@$~(@$]d@$�@#��@#{J@#j�@#U�@#A�@"��@"��@!�>@!��@!��@!Y�@ �@ �@ �@ _@ 6@ �@�@�[@��@��@1�@�@�c@�@�s@�+@�@�^@��@s�@N<@8�@+�@�P@�/@�j@�_@tT@c�@M@W?@��@h
@\�@8�@e@��@�X@��@x�@a�@�`@��@PH@@� @��@��@��@o�@RT@!-@
=@�@�]@�!@��@q�@R�@	@�)@�N@�@��@Q�@��@��@�.@r�@A�@�+@�4@$t@S@�@@�2@�<@z@M�@#:@�@{@�@�@u@�Z@�#@��@��@X@�f@�?@��@��@�4@r�@(�@�A@��@�a@�@�@�@�$@��@��@n/@o�@s@\)@�@o@�M@��@��@n�@c @0U@�-@�7@m]@e,@?}@�@�D@e�@Q�@C-@A�@C-@Ft@Ft@(�@~�@C�@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B�KB��B�B��B��B��B�sB�sB�YB��B�9B�FB�B�0B�vB�B�xB�XB��B�+B	P}B	~�B	�9B	�)B	�B	��B	�B	��B
	lB
�B
:�B
I�B
W$B
wB
��B
��B
��B
��B
�:B
��B�B�B�B-)B1�B:�BF?BlB}B��B�7BȚB��B�B�(B� B6B�B_B��B�nB�B�nB��B��B҉B��B�GB�B��B��B}�Bj�BJ�B�B
��B
�[B
�0B
�AB
i*B
XEB
CaB
%FB
�B	�HB	�-B	��B	�B	n�B	X�B	G�B	6�B	#�B	)B�RB�FB�BɆB��B��B�]B��B�B�
B��B�B�`B��B�cB�B� B�B�eB�LB��B	�B	�B	-]B	1�B	3�B	0B	(�B	(�B	/OB	%FB	�B	�B	~B�HB��B��B�DB	YB	�B	FB	3�B	M�B	W
B	[�B	`\B	h�B	n}B	{�B	�B	��B	HB	yXB	q�B	g�B	bNB	ezB	j�B	iDB	jB	l�B	o5B	u�B	�B	��B	��B	��B	� B	��B	��B	��B	�(B	��B	�uB	żB	żB	�%B	�KB	�	B	ʌB	��B	�B	οB	�(B	�\B	�B	ѷB	өB	�uB	�@B	��B	��B	ǮB	�0B	�xB	� B	��B	�+B	�
B	�?B	��B	��B	�oB	��B	� B	�(B	�pB	�pB	��B	�jB	�~B	οB	�4B	�B	�vB	ϑB	�hB	�}B	��B	��B	ѝB	�oB	�B	�B	�hB	�uB	�FB	�mB	�YB	��B	��B	��B	�_B	�B	�B	�yB	�_B	��B	��B	רB	׍B	��B	�B	ؓB	�B	�YB	�sB	�sB	�YB	ؓB	�EB	��B	��B	��B	�B	�KB	�B	�KB	�KB	�B	��B	�B	��B	ںB	ڠB	��B	��B	ڠB	�#B	��B	�CB	�)B	�)B	�CB	�]B	��B	�xB	�B	ݘB	ݲB	��B	�VB	�VB	�!B	ߊB	��B	��B	�'B	�vB	�-B	�|B	�B	�B	�B	��B	�4B	�B	�|B	�|B	�B	�B	�B	�:B	��B	�FB	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�fB	�FB	�B	�`B	�B	�B	�LB	�B	�B	�
B	�$B	�$B	�$B	�
B	�_B	�0B	�B	�B	�eB	�B	�B	��B	�WB	��B	�wB	��B	�B	�IB	��B	�wB	�wB	�B	��B	�B	�]B	�IB	�/B	�}B	�B	�/B	��B	�B	�B	��B	�B	��B	�B	�vB	�|B	�aB	�B	�MB	�hB	�B	�B	�B	��B	�%B	��B	��B	�%B	�%B	�%B	��B	��B	�B	�fB	�RB	�	B	�*B	�*B	��B	��B	��B	��B	��B	�*B	��B	�B	�B	�B	�B	��B	�jB	��B	��B	�PB	�B	�JB	��B	�dB	��B	�dB	�^B	��B	��B	�*B	�xB	��B	��B	�0B	�B	�^B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�qB	��B	�B	��B	�}B
 4B
 B
�B
�B
�B
�B
�B
gB
mB
?B
�B
EB
+B
�B
KB
�B
�B
	lB
	lB
	7B
	RB

=B
)B
B
JB
�B
�B
�B
VB
TB
,B
�B
&B
�B
�B
�B
uB
�B
,B
�B
�B
�B
?B
�B
�B
9B
B
KB
=B
IB
�B
�B
dB
IB
]B
�B
�B
dB
�B
�B
!B
�B
 �B
 �B
!�B
"hB
"�B
#B
#�B
"�B
"hB
"�B
"�B
"NB
"B
"�B
#nB
$B
$ZB
$�B
$�B
$�B
%,B
%`B
%�B
&B
&2B
&2B
%�B
$�B
&fB
&fB
&2B
%�B
&fB
&fB
&�B
'�B
'�B
(XB
(�B
)�B
*B
*�B
+B
+B
+QB
+�B
+�B
,"B
,qB
,�B
,�B
,�B
,"B
+kB
,B
-]B
-�B
.}B
.�B
.�B
.cB
./B
-CB
,�B
,�B
,�B
,�B
,�B
-]B
./B
.�B
/5B
/�B
/iB
/5B
.�B
.B
.IB
/5B
0UB
0oB
2|B
2GB
2�B
4B
4�B
5?B
5tB
5ZB
6+B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
7B
7fB
72B
7fB
7B
7fB
7�B
8B
8�B
9rB
9rB
9�B
9�B
:DB
:�B
:�B
:�B
:�B
;JB
;dB
;�B
;�B
<B
<jB
<�B
="B
=qB
=�B
=�B
=�B
>]B
>�B
?B
?�B
@ B
@OB
@�B
@�B
@�B
@�B
AB
AB
A;B
AUB
AoB
A�B
A�B
A�B
B[B
B�B
B�B
B�B
C-B
C{B
CaB
CaB
C�B
C�B
D3B
D�B
D�B
D�B
ESB
EmB
E�B
FB
F?B
FtB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
H1B
H1B
H�B
IRB
I�B
I�B
IlB
JrB
J�B
J�B
KDB
K)B
K�B
LB
LdB
LdB
L~B
L~B
L�B
M6B
M�B
M�B
N<B
NVB
N�B
OBB
O�B
O�B
PHB
PHB
P�B
P�B
Q4B
Q4B
QB
QhB
Q�B
Q�B
RB
R:B
RTB
R�B
R�B
SB
S&B
SB
S@B
S@B
S&B
SuB
S�B
T,B
TaB
T�B
T�B
U�B
U�B
U�B
VSB
V9B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
VSB
V9B
V�B
V�B
V�B
V�B
V�B
W
B
W
B
V�B
V�B
V�B
V�B
V�B
V�B
WYB
W�B
X+B
X+B
X_B
XEB
XEB
X�B
Y�B
Z7B
Z�B
Z7B
Z�B
[qB
[�B
\B
\)B
\)B
\�B
\�B
]/B
]dB
]�B
^B
^�B
_!B
_;B
_VB
_�B
_�B
_�B
`B
`vB
`�B
`�B
`�B
aB
aHB
a|B
a|B
a�B
bB
b4B
bB
b�B
cB
c B
c:B
cTB
cnB
c�B
dB
c�B
c�B
c�B
dB
dB
d&B
d&B
dtB
d�B
d�B
e,B
eFB
ezB
e�B
e�B
ffB
f�B
f�B
f�B
gRB
g8B
g8B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iB
iDB
i_B
i�B
i�B
j0B
j0B
j�B
j�B
j�B
j�B
j�B
kB
j�B
j�B
j�B
k6B
k�B
k�B
k�B
k�B
k�B
lB
l�B
mwB
mCB
mwB
m�B
ncB
n�B
o5B
o�B
o�B
pB
p!B
p!B
pUB
p;B
p�B
p�B
qB
p�B
p�B
qAB
q�B
q�B
q�B
q�B
rB
rB
rB
rGB
rGB
raB
r|B
r�B
r|B
raB
r|B
s�B
s�B
s�B
tB
tB
tB
t9B
tTB
t9B
tTB
uB
u?B
u�B
u�B
u�B
vB
vB
vFB
vFB
vFB
v�B
v�B
vzB
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{�B
|B
|B
|B
|B
|PB
|�B
|�B
}B
}B
}"B
}"B
}"B
}"B
}"B
}<B
}VB
}VB
}"B
}VB
}�B
}�B
}�B
}�B
~(B
~(B
~B
~]B
~�B
~�B
~�B
~�B
.B
.B
�B
� B
�B
�B
�B
�4B
� B
�B
� B
� B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B�=B��B�yB�EB��B��B��B��B��B��B�YB�?B�B�vB��B��B��B�B�DB�0B�lB	RB	��B	�?B	�B	��B	ٚB	�B
 4B
	�B
B
;0B
J#B
W�B
w�B
��B
�$B
��B
�7B
�B
�rBB"BB.�B3�B=�BK)Bt9B��B��B�vB��B��B��B�;B��B�B B
�BAB�+B�B�B�BیB�2BðB��B�)B�HB�hB� Bo�BP�B&�B
�B
�1B
��B
��B
l�B
\�B
HfB
(�B
	B
�B	��B	�B	��B	r|B	[�B	K^B	;dB	(�B	�B��B��B��B̈́B�*B�hB�!B��B�>B��B��B�eB�
B��B�UB�|B�GB�B�"B�RB	 iB	+B	jB	-�B	33B	5�B	2B	*eB	)�B	1�B	&�B	jB	+B	�B	 �B��B�fB�xB	�B	B	FB	3hB	M�B	W�B	\)B	`�B	h�B	n�B	{�B	��B	�9B	�B	{B	tB	h�B	cTB	f2B	k�B	i�B	kQB	m]B	o�B	u�B	��B	�MB	��B	�eB	�B	��B	��B	�VB	��B	��B	ÖB	�tB	�?B	��B	��B	�rB	��B	�xB	̳B	�B	�vB	��B	��B	�oB	�B	��B	��B	�&B	��B	�KB	�dB	�^B	� B	�1B	خB	��B	�B	յB	ӏB	��B	�TB	�NB	�\B	��B	��B	уB	��B	��B	�(B	ѷB	ЗB	ϫB	��B	��B	�B	�bB	�vB	�B	��B	҉B	�4B	�hB	өB	�{B	֡B	רB	�B	�EB	�+B	ؓB	�_B	�EB	خB	ؓB	�_B	ؓB	�+B	�+B	�_B	�yB	��B	�_B	רB	��B	��B	��B	��B	خB	�KB	��B	�KB	�B	ٴB	��B	ٴB	�B	�kB	�QB	ںB	�=B	�	B	�#B	�WB	�WB	�#B	��B	�xB	ܒB	�]B	�]B	ܒB	��B	�/B	��B	��B	�B	�5B	ޞB	��B	ߊB	�pB	��B	�'B	�'B	��B	��B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	�NB	�B	�B	�B	�@B	�zB	�B	�2B	�B	�B	��B	�B	��B	�B	��B	�B	�B	�zB	�`B	�B	�B	��B	�B	��B	��B	�>B	�>B	�XB	�sB	�XB	��B	�eB	�B	�eB	�B	�B	�B	�"B	�B	��B	��B	�cB	�B	�B	�/B	��B	��B	��B	�B	�CB	��B	�}B	�IB	�B	��B	�cB	�B	�B	�;B	�'B	��B	�'B	�[B	�B	�B	�B	�3B	�B	�B	�B	�B	�9B	��B	��B	�B	�%B	�tB	�tB	�tB	�+B	�2B	�fB	��B	��B	�XB	�^B	�^B	��B	�B	�0B	�0B	�xB	�xB	�JB	�JB	�0B	��B	��B	��B	��B	�"B	��B	��B	��B	�B	�0B	��B	�B	��B	��B	�*B	�DB	�^B	��B	��B	�B	�B	�dB	��B	�^B	��B	�B	��B	��B	��B	�6B	��B	��B	�B	��B	�<B	��B	�B	�(B	��B	��B
 iB
UB
�B
B
�B
3B
�B
�B
�B
YB
B
_B
EB
�B
�B
1B
�B
	�B
	�B
	lB
	lB

XB
DB
dB
�B
PB
B
B
<B
TB
aB
�B
uB
�B
:B
�B
�B
�B
,B
B
B
+B
�B
2B
�B
9B
7B
KB
=B
dB
�B
�B
�B
�B
�B
�B
�B
dB
�B
B
VB
 'B
 �B
 �B
!�B
"�B
#B
#TB
#�B
#:B
"�B
"�B
"�B
"�B
"hB
"�B
#�B
$ZB
$�B
$�B
%B
%B
%FB
%`B
%�B
&LB
&fB
&�B
%�B
$�B
&�B
&�B
&fB
&B
&�B
&fB
'B
'�B
(
B
(sB
(�B
*0B
*�B
+B
+6B
+B
+�B
+�B
+�B
,WB
,�B
,�B
,�B
,�B
,WB
+kB
,=B
-wB
-�B
.}B
.�B
.�B
.�B
.}B
-wB
,�B
,�B
,�B
,�B
-B
-wB
./B
.�B
/OB
/�B
/�B
/iB
/ B
.IB
.cB
/B
0oB
0�B
2�B
2aB
2�B
4B
5B
5ZB
5�B
5�B
6zB
6�B
6�B
6�B
6�B
6�B
7B
72B
72B
7�B
7fB
7�B
7LB
7�B
7�B
8RB
9	B
9�B
9�B
9�B
:B
:xB
:�B
:�B
:�B
;0B
;dB
;B
;�B
<B
<6B
<�B
=B
=<B
=�B
=�B
=�B
>BB
>�B
>�B
?cB
?�B
@4B
@OB
@�B
@�B
@�B
AB
A B
AB
AUB
A�B
A�B
A�B
A�B
BAB
B�B
B�B
CB
B�B
CGB
C{B
C{B
C{B
C�B
C�B
DgB
D�B
D�B
EB
EmB
E�B
E�B
F%B
FYB
FtB
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
HKB
HfB
IB
IlB
I�B
I�B
I�B
J�B
KB
KB
K^B
K^B
K�B
L0B
L~B
L~B
L�B
L�B
MB
M�B
M�B
N"B
N<B
N�B
N�B
OvB
O�B
PB
PbB
PHB
P�B
Q B
QNB
QNB
Q4B
Q�B
Q�B
RB
RB
RTB
R�B
R�B
R�B
S&B
S@B
S&B
S[B
S[B
S[B
S�B
S�B
TFB
T{B
T�B
UMB
U�B
U�B
U�B
VmB
VSB
V�B
V�B
V�B
W
B
W
B
W
B
V�B
VmB
VSB
V�B
V�B
W
B
V�B
V�B
W$B
W$B
W
B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
XEB
X+B
X_B
XEB
X_B
YB
Y�B
Z7B
Z�B
ZkB
Z�B
[�B
\)B
\CB
\CB
\]B
\�B
\�B
]dB
]�B
]�B
^B
_B
_;B
_;B
_pB
_�B
_�B
`'B
`\B
`�B
`�B
`�B
aB
aHB
abB
a�B
a�B
a�B
bB
b4B
b4B
b�B
cB
c:B
c:B
cnB
c�B
c�B
d&B
c�B
c�B
c�B
d@B
d@B
d&B
d@B
d�B
d�B
eB
eFB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gRB
gRB
gRB
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
jB
jKB
jeB
j�B
j�B
kB
j�B
kB
kB
kB
kB
kB
kQB
k�B
k�B
k�B
lB
k�B
l=B
l�B
m�B
m]B
m�B
m�B
n}B
n�B
oOB
o�B
pB
p!B
p;B
pB
pUB
pUB
p�B
p�B
qB
qB
q'B
qvB
q�B
q�B
q�B
q�B
rB
rB
r-B
raB
raB
r|B
r|B
r�B
r�B
r�B
r�B
s�B
s�B
tB
tB
tB
tB
tTB
tTB
tTB
t�B
u%B
uZB
u�B
u�B
u�B
vB
v+B
v`B
v`B
vFB
v�B
v�B
vzB
v�B
v�B
v�B
wB
wB
w�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
zB
zB
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{�B
|B
|B
|B
|6B
|PB
|�B
|�B
}B
}B
}"B
}"B
}"B
}B
}"B
}<B
}<B
}<B
}<B
}qB
}�B
}�B
}�B
~B
~(B
~BB
~(B
~�B
B
~�B
~�B
B
HB
cB
�B
� B
�B
� B
�B
�4B
�B
�B
�4B
�;B
� B
� 31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005290056252020052900562520200529005625202005290202252020052902022520200529020225202207271538172022072715381720220727153817  JA  ARFMdecpA30a                                                                20200518093755  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200518093804  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200518093805  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200518093806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200518093806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200518093806  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200518093806                      G�O�G�O�G�O�                JA  ARUP                                                                        20200518095402                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200519000000  CF  PSAL_ADJUSTED_QC@@G�O�                JM  ARCAJMQC2.0                                                                 20200528155625  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200528155625  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200528170225  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063817  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                