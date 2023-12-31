CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-11T00:35:40Z creation;2018-04-11T00:35:45Z conversion to V3.1;2019-12-19T07:41:01Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180411003540  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_229                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�Z5��s�1   @�Z6K� @4�1&�x��dIVl�!1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�@�Q�A(�A<(�A\(�A|(�A�{A�{A��HA�{A�{A�{A�{A�{B
=B
=B
=B
=B'
=B/
=B7
=B?
=BG
=BO
=BW
=B_
=Bg
=Bo
=Bw
=B
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3�)C5�)C7�)C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�=D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJj=DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��D��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD���D��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�uD޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�{�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A�{A�{A�{A�oA�{A�oA�oA�{A�bA�VA�VA�1A�
=A�1A�%A��HA���A�bA�ȴAȃA�;dA�%A��
A���AǼjA�v�A�ZA�
=AƮA��AŲ-A�E�A���A�A��mAĝ�Aģ�Aĕ�A�%AþwA�l�A�E�A���A©�ADA�^5A��A��`A�1A�E�A��
A�1'A�\)A�jA���A��A��A�oA�l�A���A�oA�+A�p�A��A�A���A�Q�A�\)A�ZA�O�A��`A�-A�O�A�O�A�hsA��A���A�t�A��A��A���A��A��^A�ĜA�$�A���A�r�A�oA��^A�;dA��#A��DA�"�A��A�$�A��A�ƨA�33A�33A��
A�ĜA��mA���A�S�A��A���A���A�A�A�VA�5?A��\A���A�O�A��FA�`BA��A}VAx��Au\)ArbNAq�hAp��ApE�Aop�An�!Am7LAljAk��AiVAg�wAf�`Ad�A`^5A[C�AX��AV�AT�ASC�AQ�mAP��AO�AO+ALA�AK�AJ��AIAG��AFVAEt�AE%AD��AD��AD{AB�/A?�FA=G�A:=qA9%A7XA6-A5��A3��A2��A1oA/�A/7LA.�A-%A*Q�A)ƨA(�HA%�;A$jA#�FA"ĜA"1A �`A;dAE�A�RA&�A��A�Az�A�;A�+A
=A5?AO�Ar�A{A�A\)A�9A��A��A33A
��A
A�A�A1'AdZA��A/AVA�+At�A��A9XA��AK�A �/@���@�9X@���@��h@���@��!@�X@��u@�v�@�O�@� �@�P@�!@��T@���@�33@�J@�Q�@��;@�@�+@�@��@���@�|�@�@�{@�j@��
@�33@�o@��@ާ�@�n�@�@��`@ۅ@��@�$�@��@أ�@�S�@��H@���@�^5@�@Ցh@�?}@��@�t�@���@��@�Ĝ@��m@�;d@Ͳ-@�9X@��@ɺ^@�Ĝ@���@���@���@ļj@�Z@�t�@+@��@��@���@��@��@��+@�@��9@�Q�@�9X@�1@�\)@��h@��D@�Z@�1'@�  @�ȴ@�-@���@���@��h@�G�@�%@��j@�"�@��\@�n�@�5?@�E�@��#@�@�X@�V@���@���@���@�Z@�  @�\)@�+@��y@���@�v�@��@��T@�@�&�@��`@��u@�(�@�|�@�o@��@��@��y@���@���@�=q@��@���@�%@���@�Q�@��@��
@�|�@��@�M�@���@���@��@�z�@� �@� �@��@� �@��w@�t�@�o@��@��+@��\@�^5@�@�@���@���@���@�bN@� �@�  @�Q�@��@���@�l�@�K�@�dZ@�S�@�K�@���@���@���@�ȴ@��R@��+@���@��\@�^5@���@���@��!@�J@���@�X@�?}@��@���@�9X@�1@���@��@�l�@�C�@�"�@���@�ȴ@���@�ff@��@��^@��7@�p�@�`B@�X@�/@��@��9@���@��@��u@��m@��;@��F@���@���@�|�@�dZ@�K�@�o@���@��!@���@�$�@��@�p�@�X@�7L@�V@��`@��j@���@�z�@�(�@��F@���@�S�@��y@�v�@�=q@��^@�7L@�?}@�7L@�?}@�?}@�G�@�G�@�7L@�V@���@�Ĝ@� �@��@�33@�+@�+@�33@�+@�+@�"�@�
=@���@�@��@��^@���@�V@���@��j@���@��D@��@�z�@�I�@�1'@�(�@��@�1@��;@��w@��P@�dZ@�\)@�+@��y@��+@�v�@�ff@�ff@�^5@�E�@�5?@�$�@�@���@��h@��@�x�@�p�@�X@���@�Ĝ@�j@�b@��;@��F@���@�S�@�o@�@��y@��y@��y@��@���@��+@�n�@�^5@�5?@��@��^@�`B@��@���@���@��j@��9@�j@�(�@��
@��w@��@��@��P@�33@�@��H@��@��!@�$�@��@�@��-@��@��@��9@���@�A�@�1@��@l�@\)@;d@~��@}�-@}`B@}?}@|�@|I�@{��@{��@{S�@{33@{"�@{"�@{o@{@z�@z~�@y��@y7L@x�u@w�@wl�@w
=@v�@v��@vE�@v$�@u�T@u�@u?}@u�@t�j@tz�@t(�@s�
@st�@r�!@qX@p��@p�9@p��@p�@pbN@pQ�@p1'@pb@o�@o�;@o�w@ol�@o;d@o
=@n�@nE�@l��@l(�@l1@k�m@kƨ@kt�@j�!@i�@hb@g��@g�@g\)@g+@f��@fȴ@fff@f@e��@e/@d��@dz�@dZ@c��@c�m@c�
@c�F@c�@cdZ@c33@c@b�\@bn�@a��@aG�@`�`@`�9@`bN@_�@_K�@_�@^��@^��@]�@]`B@]V@\�/@\��@\z�@\I�@[ƨ@[t�@[33@Z��@Z=q@Y��@Yhs@Y�@X��@X�@X �@W�P@W
=@V�@Vȴ@VV@U�@U�@UV@T�j@T�@TZ@T1@S�m@St�@S@R��@R~�@Q�^@QG�@P��@P��@Pr�@O�@O;d@N��@Nff@NV@N5?@N$�@N{@N@M�@L�@L�@Lz�@K�
@K33@J��@J�!@J�\@JM�@JJ@I��@I�7@Hr�@H  @G��@G;d@F��@F@E��@E@E�-@E�@D��@D�@D��@D�D@DI�@D9X@C��@CdZ@B�@B��@B�@A��@A�#@A��@A��@A��@A��@Ax�@@�u@@b@?�@?�;@?�;@?�w@?l�@?
=@>�R@>�+@>�+@>�+@>v�@>v�@>E�@>@=�-@=O�@=�@=V@<��@<z�@<(�@<�@<1@;�m@;S�@:�@:��@:��@:~�@:^5@:-@9�@9��@97L@8��@8Ĝ@8��@8�@8bN@8 �@7�w@7�@7�P@6�@6$�@5�-@5/@4��@4��@4�@4z�@3�m@3t�@3S�@3C�@3@2��@2=q@1�@1�#@1�#@1��@1G�@1G�@1G�@1�@0��@0��@0Q�@0  @/�w@/�@/K�@/
=@.�y@.��@.E�@.@-�T@-�-@-�h@-�@-�@-`B@,��@,��@,z�@,1@+ƨ@+�@*�H@*�\@*~�@*n�@*^5@*^5@*^5@*^5@*M�@*=q@)��@)��@)x�@)X@)%@(�9@(Q�@'��@'�P@'K�@&�@&��@&v�@&E�@&5?@&{@%�@%V@$�/@$�@$z�@$Z@$(�@#��@#�
@#�F@#�@#S�@#o@"��@"�!@"^5@"�@"J@!��@!�@!��@!�^@!��@!�7@!G�@ �`@ Ĝ@ �u@ �u@ �@ �@ r�@ bN@ Q�@�@��@|�@;d@�@E�@$�@{@�@�-@�@p�@/@V@�/@��@j@�@��@ƨ@��@�@t�@S�@o@@�H@��@��@^5@=q@�@��@x�@%@��@Ĝ@bN@1'@b@�;@��@��@l�@K�@��@�@ȴ@ȴ@�@ȴ@�R@��@V@{@�T@��@�-@�@`B@/@�@�@��@�D@z�@(�@�m@ƨ@��@t�@t�@S�@@��@��@~�@n�@=q@�@�#@��@�^@x�@G�@&�@&�@�`@��@r�@r�@A�@b@�@�P@�@�@��@v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A�{A�{A�{A�oA�{A�oA�oA�{A�bA�VA�VA�1A�
=A�1A�%A��HA���A�bA�ȴAȃA�;dA�%A��
A���AǼjA�v�A�ZA�
=AƮA��AŲ-A�E�A���A�A��mAĝ�Aģ�Aĕ�A�%AþwA�l�A�E�A���A©�ADA�^5A��A��`A�1A�E�A��
A�1'A�\)A�jA���A��A��A�oA�l�A���A�oA�+A�p�A��A�A���A�Q�A�\)A�ZA�O�A��`A�-A�O�A�O�A�hsA��A���A�t�A��A��A���A��A��^A�ĜA�$�A���A�r�A�oA��^A�;dA��#A��DA�"�A��A�$�A��A�ƨA�33A�33A��
A�ĜA��mA���A�S�A��A���A���A�A�A�VA�5?A��\A���A�O�A��FA�`BA��A}VAx��Au\)ArbNAq�hAp��ApE�Aop�An�!Am7LAljAk��AiVAg�wAf�`Ad�A`^5A[C�AX��AV�AT�ASC�AQ�mAP��AO�AO+ALA�AK�AJ��AIAG��AFVAEt�AE%AD��AD��AD{AB�/A?�FA=G�A:=qA9%A7XA6-A5��A3��A2��A1oA/�A/7LA.�A-%A*Q�A)ƨA(�HA%�;A$jA#�FA"ĜA"1A �`A;dAE�A�RA&�A��A�Az�A�;A�+A
=A5?AO�Ar�A{A�A\)A�9A��A��A33A
��A
A�A�A1'AdZA��A/AVA�+At�A��A9XA��AK�A �/@���@�9X@���@��h@���@��!@�X@��u@�v�@�O�@� �@�P@�!@��T@���@�33@�J@�Q�@��;@�@�+@�@��@���@�|�@�@�{@�j@��
@�33@�o@��@ާ�@�n�@�@��`@ۅ@��@�$�@��@أ�@�S�@��H@���@�^5@�@Ցh@�?}@��@�t�@���@��@�Ĝ@��m@�;d@Ͳ-@�9X@��@ɺ^@�Ĝ@���@���@���@ļj@�Z@�t�@+@��@��@���@��@��@��+@�@��9@�Q�@�9X@�1@�\)@��h@��D@�Z@�1'@�  @�ȴ@�-@���@���@��h@�G�@�%@��j@�"�@��\@�n�@�5?@�E�@��#@�@�X@�V@���@���@���@�Z@�  @�\)@�+@��y@���@�v�@��@��T@�@�&�@��`@��u@�(�@�|�@�o@��@��@��y@���@���@�=q@��@���@�%@���@�Q�@��@��
@�|�@��@�M�@���@���@��@�z�@� �@� �@��@� �@��w@�t�@�o@��@��+@��\@�^5@�@�@���@���@���@�bN@� �@�  @�Q�@��@���@�l�@�K�@�dZ@�S�@�K�@���@���@���@�ȴ@��R@��+@���@��\@�^5@���@���@��!@�J@���@�X@�?}@��@���@�9X@�1@���@��@�l�@�C�@�"�@���@�ȴ@���@�ff@��@��^@��7@�p�@�`B@�X@�/@��@��9@���@��@��u@��m@��;@��F@���@���@�|�@�dZ@�K�@�o@���@��!@���@�$�@��@�p�@�X@�7L@�V@��`@��j@���@�z�@�(�@��F@���@�S�@��y@�v�@�=q@��^@�7L@�?}@�7L@�?}@�?}@�G�@�G�@�7L@�V@���@�Ĝ@� �@��@�33@�+@�+@�33@�+@�+@�"�@�
=@���@�@��@��^@���@�V@���@��j@���@��D@��@�z�@�I�@�1'@�(�@��@�1@��;@��w@��P@�dZ@�\)@�+@��y@��+@�v�@�ff@�ff@�^5@�E�@�5?@�$�@�@���@��h@��@�x�@�p�@�X@���@�Ĝ@�j@�b@��;@��F@���@�S�@�o@�@��y@��y@��y@��@���@��+@�n�@�^5@�5?@��@��^@�`B@��@���@���@��j@��9@�j@�(�@��
@��w@��@��@��P@�33@�@��H@��@��!@�$�@��@�@��-@��@��@��9@���@�A�@�1@��@l�@\)@;d@~��@}�-@}`B@}?}@|�@|I�@{��@{��@{S�@{33@{"�@{"�@{o@{@z�@z~�@y��@y7L@x�u@w�@wl�@w
=@v�@v��@vE�@v$�@u�T@u�@u?}@u�@t�j@tz�@t(�@s�
@st�@r�!@qX@p��@p�9@p��@p�@pbN@pQ�@p1'@pb@o�@o�;@o�w@ol�@o;d@o
=@n�@nE�@l��@l(�@l1@k�m@kƨ@kt�@j�!@i�@hb@g��@g�@g\)@g+@f��@fȴ@fff@f@e��@e/@d��@dz�@dZ@c��@c�m@c�
@c�F@c�@cdZ@c33@c@b�\@bn�@a��@aG�@`�`@`�9@`bN@_�@_K�@_�@^��@^��@]�@]`B@]V@\�/@\��@\z�@\I�@[ƨ@[t�@[33@Z��@Z=q@Y��@Yhs@Y�@X��@X�@X �@W�P@W
=@V�@Vȴ@VV@U�@U�@UV@T�j@T�@TZ@T1@S�m@St�@S@R��@R~�@Q�^@QG�@P��@P��@Pr�@O�@O;d@N��@Nff@NV@N5?@N$�@N{@N@M�@L�@L�@Lz�@K�
@K33@J��@J�!@J�\@JM�@JJ@I��@I�7@Hr�@H  @G��@G;d@F��@F@E��@E@E�-@E�@D��@D�@D��@D�D@DI�@D9X@C��@CdZ@B�@B��@B�@A��@A�#@A��@A��@A��@A��@Ax�@@�u@@b@?�@?�;@?�;@?�w@?l�@?
=@>�R@>�+@>�+@>�+@>v�@>v�@>E�@>@=�-@=O�@=�@=V@<��@<z�@<(�@<�@<1@;�m@;S�@:�@:��@:��@:~�@:^5@:-@9�@9��@97L@8��@8Ĝ@8��@8�@8bN@8 �@7�w@7�@7�P@6�@6$�@5�-@5/@4��@4��@4�@4z�@3�m@3t�@3S�@3C�@3@2��@2=q@1�@1�#@1�#@1��@1G�@1G�@1G�@1�@0��@0��@0Q�@0  @/�w@/�@/K�@/
=@.�y@.��@.E�@.@-�T@-�-@-�h@-�@-�@-`B@,��@,��@,z�@,1@+ƨ@+�@*�H@*�\@*~�@*n�@*^5@*^5@*^5@*^5@*M�@*=q@)��@)��@)x�@)X@)%@(�9@(Q�@'��@'�P@'K�@&�@&��@&v�@&E�@&5?@&{@%�@%V@$�/@$�@$z�@$Z@$(�@#��@#�
@#�F@#�@#S�@#o@"��@"�!@"^5@"�@"J@!��@!�@!��@!�^@!��@!�7@!G�@ �`@ Ĝ@ �u@ �u@ �@ �@ r�@ bN@ Q�@�@��@|�@;d@�@E�@$�@{@�@�-@�@p�@/@V@�/@��@j@�@��@ƨ@��@�@t�@S�@o@@�H@��@��@^5@=q@�@��@x�@%@��@Ĝ@bN@1'@b@�;@��@��@l�@K�@��@�@ȴ@ȴ@�@ȴ@�R@��@V@{@�T@��@�-@�@`B@/@�@�@��@�D@z�@(�@�m@ƨ@��@t�@t�@S�@@��@��@~�@n�@=q@�@�#@��@�^@x�@G�@&�@&�@�`@��@r�@r�@A�@b@�@�P@�@�@��@v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
hsB
hsB
hsB
iyB
hsB
gmB
cTB
dZB
��B
�B
��B%BbB�B#�B(�B(�B8RB9XB@�BE�BJ�BXBhsB� B��B��B�'B�jB�LB��B�;B��B��BB�B�B�B{BVBDB�B;dB7LB/B49B&�B-B:^B>wB@�B>wB7LBA�BE�BI�B1'B�BB��B��B�BJB  B�B�B��B�B�B�;B�
BȴB�LB��B�B;dBL�BdZBbNB^5BT�BP�BK�BB�B7LB.BuBVB�B�B�B+B
�yB
��B
o�B
�=B
y�B
I�B
 �B
$�B
:^B
=qB
<jB
=qB
8RB
49B
'�B
B	��B	��B	��B	�TB	�HB	�#B	��B	��B	�qB	�RB	�!B	�\B	�7B	� B	]/B	'�B		7B	�B	�B	�B	�B	�B	uB	hB	VB�B��B	  B��B�TB�B�NB�mB�mB�BB��B�wB��B��B�=B��B��B��B��B�=B�\B�1B�1B�PB�DBt�B`BB{�Bq�BP�BdZBo�Bk�Be`B`BBO�B[#BN�BM�BQ�BZBXBXBG�BH�BL�BM�BJ�B7LBZBYBR�BI�BH�BG�BS�BVBG�BJ�BI�B?}BG�BR�BJ�BB�BG�BJ�BK�BL�BE�B8RB>wBF�BM�BN�BE�BL�BR�BJ�BP�BT�BZBXBXBP�BW
BT�BQ�B]/B[#B^5B]/B^5B_;B`BB`BB]/B[#BcTBffBl�Bl�BjBiyBe`BcTB`BBjBiyBiyBo�Bk�Bt�By�Bv�Bv�Bw�Bv�Bs�Bv�By�Bw�Bv�B|�B�By�B{�B�B�7B�bB�oB�VB��B��B��B��B��B��B��B�B�RB�RB�^B�^B�dBÖBȴBƨBÖBB��B�5B�5B�/B�
B�HB�sB�B�B�B�B�sB�TB�B��B��B��B��B��B��B��B	  B	B	B	B	B	B	DB	DB	PB	PB	\B	uB	{B	uB	�B	�B	�B	�B	�B	'�B	(�B	(�B	(�B	+B	(�B	(�B	+B	)�B	,B	0!B	0!B	33B	2-B	2-B	7LB	>wB	@�B	@�B	D�B	K�B	P�B	S�B	T�B	VB	YB	YB	\)B	^5B	e`B	iyB	jB	o�B	o�B	l�B	iyB	n�B	u�B	z�B	�B	~�B	� B	�1B	�7B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�9B	�'B	�3B	�?B	�RB	�LB	�RB	�XB	�qB	�wB	�}B	��B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�#B	�B	�B	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�`B	�ZB	�ZB	�B	�yB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
+B
1B
+B
%B
%B

=B
DB
DB
DB
DB
JB
JB
JB
DB
VB
\B
\B
\B
VB
JB
JB
PB
PB
\B
bB
hB
\B
bB
uB
uB
uB
uB
oB
hB
oB
oB
oB
hB
bB
hB
hB
oB
{B
{B
�B
�B
uB
uB
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
$�B
#�B
#�B
$�B
$�B
#�B
$�B
$�B
#�B
#�B
"�B
 �B
%�B
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
&�B
&�B
%�B
%�B
#�B
"�B
$�B
(�B
(�B
'�B
%�B
#�B
!�B
#�B
(�B
)�B
(�B
)�B
+B
+B
)�B
)�B
)�B
+B
+B
.B
.B
-B
0!B
/B
/B
/B
0!B
/B
/B
.B
/B
.B
-B
.B
0!B
/B
/B
/B
1'B
1'B
0!B
/B
1'B
2-B
33B
33B
49B
33B
2-B
33B
49B
33B
33B
33B
5?B
5?B
5?B
5?B
49B
49B
5?B
7LB
7LB
6FB
6FB
6FB
7LB
7LB
9XB
8RB
8RB
9XB
8RB
7LB
9XB
9XB
8RB
:^B
;dB
<jB
<jB
;dB
:^B
<jB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
;dB
>wB
?}B
=qB
=qB
?}B
A�B
A�B
@�B
@�B
@�B
?}B
<jB
@�B
A�B
@�B
@�B
A�B
D�B
E�B
D�B
C�B
B�B
C�B
E�B
E�B
D�B
D�B
C�B
B�B
C�B
D�B
D�B
F�B
F�B
F�B
G�B
G�B
F�B
E�B
C�B
D�B
G�B
G�B
G�B
F�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
G�B
H�B
H�B
H�B
F�B
G�B
I�B
I�B
H�B
G�B
H�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
K�B
M�B
L�B
M�B
M�B
L�B
L�B
M�B
L�B
J�B
I�B
K�B
L�B
N�B
O�B
O�B
O�B
N�B
O�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
S�B
S�B
S�B
R�B
T�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
W
B
VB
VB
W
B
VB
VB
W
B
XB
XB
XB
YB
YB
YB
XB
XB
YB
XB
ZB
YB
YB
[#B
\)B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
[#B
\)B
\)B
\)B
[#B
\)B
[#B
[#B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
^5B
]/B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
bNB
bNB
dZB
dZB
cTB
cTB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
gmB
ffB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
gmB
hsB
jB
iyB
hsB
jB
k�B
k�B
l�B
k�B
k�B
k�B
k�B
m�B
m�B
n�B
m�B
m�B
m�B
l�B
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
p�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
q�B
q�B
s�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
r�B
t�B
s�B
t�B
t�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
iyB
h�B
g�B
d@B
f�B
��B
��B
��B�B�BB$B)*B)�B8�B:DBA�BF�BK�BX�Bi_B�OB��B��B�[B��B��B�xB��B�B��B�B�B1BYB�B�B�B�B<�B9>B1'B5�B)�B/5B;�B@ BA�B@�B9rBCBF�BJ�B3�BdB�B�^B��B�B<B'B�B�B��B��B��B�BBؓBʌB��B��B�KBB�BO\BeBcTB_;BV9BQ�BL�BC�B8�B/�B�BB;B�BB	�B
�B
�?B
v�B
�PB
|�B
O(B
&B
'�B
<6B
?.B
>BB
>�B
9�B
5tB
)�B
KB	�eB	��B	� B	�ZB	�4B	�CB	�2B	�B	�.B	��B	��B	��B	�B	��B	`�B	-�B	(B	�B	�B	!�B	�B	_B	B	�B	�B�B�PB	 �B�8B��B�)B�nB�
B��B��B�FB��B�B��B��B��B��B�OB��B��B� B�XB��B�pB�~Bw�Bc�B|�Bs�BT�Bf2Bp�Bl�Bf�Ba�BRoB\�BQBPBS�B[#BY1BYKBI�BJ�BN"BOvBLdB:�BZ�BY�BT,BKxBJXBI�BT�BV�BI�BK�BKDBAoBH�BSuBK�BDBH�BK�BL�BM�BF�B:�B@4BG�BN�BO�BG+BM�BS�BLJBQ�BVBZ�BX�BX�BRTBW�BVBS&B]�B[�B^�B^B^�B_�B`�B`�B^5B\CBd&Bf�Bl�Bl�BkBi�BfBd&Ba|BkBj0BjKBp!Bl�Bu?Bz*BwLBwLBxRBwfBt�Bw�Bz�Bx�Bw�B}�B��B{0B}"B�%B�XB�4B�[B��B�IB�2B�`B��B��B��B��B��B��B�	B��B�B�PB��B��B�BāB��BЗB�jBބBݘB��B��B��B��B��B�B��B��B�B�!B�	B�$B�"B�>B�<B�PB�BB	 OB	�B	mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	B	)B	CB	 BB	($B	)*B	)*B	)_B	+QB	)_B	)_B	+kB	*�B	,qB	0�B	0�B	3hB	2�B	2�B	7�B	>�B	AB	AB	EB	LB	Q B	T,B	U2B	VmB	YB	YB	\xB	^�B	ezB	i�B	j�B	o�B	o�B	mB	jKB	o B	vB	{0B	�B	cB	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�QB	�IB	�-B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�(B	�4B	� B	�TB	ϑB	�B	�SB	�EB	�KB	�KB	�QB	�kB	چB	�qB	�]B	�qB	ڠB	ںB	�nB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�;B	�MB	�DB	�B	�B	�B	�B	�B	�B	�*B	�lB	�RB	�"B	�PB	�6B	�JB	�<B	�.B	�HB	�.B
 4B
 OB
 OB
;B
[B
AB
aB
{B
SB
mB
_B
fB
zB
tB
�B

rB
xB
xB
xB
xB
~B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
)B
)B
�B
�B
B
B
�B
 B
 �B
!�B
"�B
"�B
!�B
"B
!B
B
B
B
 B
 'B
"B
#B
$B
$B
$B
%B
$B
$&B
%B
%B
$@B
%,B
%,B
$&B
$@B
#TB
!bB
&B
(
B
(>B
($B
($B
($B
($B
($B
($B
($B
($B
'8B
'B
&B
&B
$@B
#TB
%`B
)B
)*B
($B
&2B
$ZB
"�B
$ZB
)*B
*KB
)*B
*KB
+6B
+6B
*KB
*KB
*KB
+QB
+QB
./B
.IB
-CB
0;B
/OB
/OB
/iB
0UB
/OB
/OB
.cB
/OB
.cB
-�B
.cB
0oB
/iB
/�B
/iB
1[B
1[B
0�B
/�B
1vB
2aB
3hB
3hB
4nB
3hB
2|B
3�B
4nB
3�B
3�B
3�B
5tB
5tB
5tB
5tB
4�B
4�B
5�B
7�B
7�B
6�B
6�B
6�B
7�B
7�B
9rB
8�B
8�B
9�B
8�B
7�B
9�B
9�B
8�B
:�B
;�B
<�B
<�B
;�B
:�B
<�B
>�B
?�B
?�B
?�B
?�B
?�B
>�B
;�B
>�B
?�B
=�B
=�B
?�B
A�B
A�B
@�B
@�B
@�B
?�B
=B
@�B
A�B
@�B
@�B
A�B
D�B
E�B
D�B
C�B
B�B
C�B
E�B
E�B
D�B
D�B
C�B
B�B
C�B
D�B
D�B
F�B
F�B
F�B
G�B
G�B
F�B
E�B
C�B
D�B
G�B
G�B
G�B
F�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
G�B
IB
IB
H�B
F�B
G�B
I�B
I�B
H�B
G�B
H�B
J�B
J�B
K�B
J�B
KB
J�B
KB
KB
K�B
M�B
MB
NB
NB
MB
MB
NB
MB
K)B
J=B
LB
MB
OB
PB
PB
PB
OBB
P.B
RB
R:B
R B
Q4B
R B
R:B
T,B
TB
T,B
S&B
UB
UB
T,B
T,B
T,B
T,B
U2B
UMB
W$B
V9B
V9B
WYB
V9B
V9B
W?B
X_B
XEB
XEB
Y1B
Y1B
YKB
X_B
XEB
YKB
XyB
ZQB
YeB
YeB
[WB
\CB
]dB
]IB
]dB
]IB
]IB
\]B
\]B
[qB
\]B
\]B
\]B
[WB
\xB
[�B
[qB
]dB
]dB
]�B
^�B
_pB
_pB
_pB
^jB
]~B
^�B
_pB
`�B
`vB
`vB
a|B
a|B
a|B
a|B
a�B
a�B
a|B
a�B
b�B
a�B
a|B
bhB
bhB
bhB
b�B
bhB
b�B
b�B
a�B
a|B
cnB
c�B
dtB
dtB
dtB
dtB
dtB
c�B
b�B
b�B
d�B
d�B
c�B
c�B
f�B
f�B
f�B
f�B
f�B
g�B
f�B
f�B
f�B
f�B
g�B
f�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
h�B
i�B
i�B
i�B
i�B
g�B
h�B
j�B
i�B
h�B
j�B
k�B
k�B
l�B
k�B
k�B
k�B
k�B
m�B
m�B
n�B
m�B
m�B
m�B
l�B
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
p�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
q�B
q�B
s�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
r�B
uB
tB
t�B
uB
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.24(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804150037222018041500372220180415003722201806221328552018062213285520180622132855201804261707282018042617072820180426170728  JA  ARFMdecpA19c                                                                20180411093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180411003540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180411003542  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180411003543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180411003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180411003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180411003543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180411003543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180411003545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180411003545                      G�O�G�O�G�O�                JA  ARUP                                                                        20180411005702                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180411153643  CV  JULD            G�O�G�O�F�ѭ                JM  ARGQJMQC2.0                                                                 20180411153643  CV  JULD_LOCATION   G�O�G�O�F�Ѻ                JM  ARGQJMQC2.0                                                                 20180411153643  CV  LATITUDE        G�O�G�O�A�E�                JM  ARCAJMQC2.0                                                                 20180414153722  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180414153722  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426080728  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042855  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                