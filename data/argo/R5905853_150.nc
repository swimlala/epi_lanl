CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-20T06:46:09Z creation;2023-03-20T06:46:15Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230320064609  20230320070027  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�C��H1   @�D`T�@0$�/���cM���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B�  B���B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C633C8  C9�fC<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @
=q@w
=@�Q�@�Q�A�\A<(�A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
=B
=B
=B'
=B/
=B7
=B?p�BG
=BO
=BW
=B_
=Bg
=Bo
=Bw
=B
=B��B��B��B��B��B��B��B�Q�B��RB��B�Q�B��B��B��B��B��B�Q�BǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCC�)C�)C	CCCCCCCCCCC��C!C#C%C'C)C+C-C/C1C3�)C5��C7C9��C;C=C?��CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCu�)CwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dw
D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+w
D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|w
D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�xRD޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��>A̹�A�D�A��<Aˀ�A�K�A�.A��A�A�A��A���A��A�X�A�5A��Aƙ�A�7�A��Aű[AşVA�cA�gmA�M�A�_A�MA��.A���A��8A���AĽAİ!Aİ!AĵAĒ�A�49A��A�PA�0!A�,=A��QA�:^A��EA�/�A�W�A�W�A��QA���A��1A���A���A�A��A�GA�\�A��A�?�A�YA���A��eA��	A�XEA�rGA���A���A��A���A��A��A���A���A��/A���A� �A�d�A�c�A�9XA���A�I�A���A�gmA��A���A��nA�u�A�֡A���A��A|�!A{U2Aw2�As�MAo��An)_AmAjK^Ag�dAe�SA`@�A\4AX_AS��AM��AH��AEIRAD��AC�9AB��AB�AAj�AA�)A@{A?(A>�'A>	lA<��A<��A<J#A9RTA4qA2��A1S�A/X�A-&�A,�MA,�zA-��A/[�A.�sA-�kA+��A*�3A)��A' �A$�8A$�#A!eA�A�A��AD�A�AߤA�A�rA1�A�|A��A~�A�'A_A��Ay�AS&AC-A%A�AiDA5?A�zA�A}VAqA�#A��A�A�A��AC-A�A�A^5AU2A�AdZA]�A�sAbNA��A�A8A�\A��A�}A��A;�A�NA_A
�uA	]dA��AHA�As�A^5AFA7�AjA�AkQA;dA�A�'A�>A-�A cA `BA q@��F@�{�@�!-@��@��A@�S�@��@�H�@��]@�y>@��@���@��V@��$@�l�@�s@��@���@���@�m]@�B�@�q@�%F@�@�l"@�N�@��@�+@���@��@��m@��@쀝@�@�|�@�&�@�U�@�ی@�S&@���@�F@�@��]@藍@�)�@�S@�O�@�ں@�l"@�w2@���@�7@�}@�<�@�E9@�L@�8�@߳�@� i@���@���@��@އ+@�m�@��@ެ�@��@ީ�@�#:@�,�@��@ݐ�@ݖS@܋D@��&@�L0@ې�@�V�@ي	@�&�@��Q@�:*@ջ0@Վ�@�8@�s�@��@�z�@ָR@��/@��@�ی@�l�@Ӌ�@��	@үO@���@ҧ@���@�!�@�f�@��@Μx@Β�@�z�@��@�xl@�rG@�?}@��@���@ʒ�@��@�y>@�d�@��@�ݘ@�6z@��@���@ǀ4@�RT@�C@ƣ�@��&@ğ�@� �@Ý�@F@�E�@���@�E�@�qv@���@�:�@���@��@���@��s@�N<@�t�@�;d@�ں@�z�@���@��@���@�M@���@��_@�'R@��D@���@�@���@��u@�,=@��F@�Dg@��e@�e�@�M�@�>B@��#@�Z�@�5�@��@��/@��\@��9@���@�S@�~�@�@���@�zx@�RT@��@�l"@�1'@�	@�8�@��
@��$@�9�@��@���@��j@�q�@�+k@��@��]@��@���@�y�@��@��m@��+@�D�@�M@���@�s�@��@��#@�N<@�8@�IR@��E@�e�@�h
@�:*@��@�O�@�@�?}@��p@�M�@��@��A@���@���@�s@�.I@��@���@�l�@�:�@��@��@��9@��=@�F@�҉@��L@�m�@�D�@��@��w@���@��@�+@��`@�E�@���@�ϫ@��@��@�V@��@���@�o @���@�!@�y�@�q@��f@��@�4@��-@��@�c@�A @�o@��$@�~�@��A@��	@��U@�Q�@�e@���@�^�@�IR@�P�@�H�@�=�@�5�@�.I@�-w@�)_@�@���@���@�g8@���@�}�@�x�@��$@��h@�n/@�33@��@��`@���@�u%@�Ft@�	@��@��^@�e�@�+�@���@�tT@�($@��j@��S@�P�@��@��@��L@�q�@�	@��=@�e,@�\)@�2a@��5@�҉@��@��@���@��u@�q@�_@�+k@��@�s@�O�@�%F@��@���@�%�@��Q@�dZ@�A�@�!-@��@�]d@��)@��@��3@��@��4@�[W@���@���@�e�@� �@��@���@��H@���@�l�@�=�@���@���@�L0@��j@�e,@�O�@�)_@��|@���@�q�@�+k@��@��d@���@��g@���@�`B@�N<@�A�@�@��P@��6@���@�a|@�
@'�@~h
@}��@}�@}�@|A�@{�f@{>�@{�@z�r@z��@z��@z�<@z��@z�@yf�@x:�@w�g@w��@v�M@vȴ@v��@uIR@s�@st�@s�@rxl@q�@p�$@o�@n��@m��@m��@lM@k��@k�@j͟@j��@jZ�@jR�@j&�@i�j@j �@i��@i�M@h��@gj�@g�F@g�@@g�f@g�@gU�@f��@e��@e%F@de�@d:�@d�@c�W@b��@b0U@a��@a��@a;@`��@`m�@`M@`�@_x@^�y@^��@^O@]��@]o @\~(@\/�@\�@\S�@\9X@[��@[��@[�q@[v`@[P�@["�@[�@[y�@[H�@[o@Z�y@Z�\@Y��@Y�@Y�C@Y�^@Yzx@YQ�@Y:�@Yc�@YIR@X��@Xw�@X�.@X��@X�v@X��@Xu�@XD�@X!@X�@W��@Wb�@W�@V�@V��@U�@T:�@Sqv@SO@SP�@S;d@S�@S(@R�\@R@Q��@Qm]@Q \@P��@Pی@P�E@P��@O�
@OJ#@N��@NTa@Mp�@L�v@L�@LN�@K�@Kv`@J�8@Jp;@J-@I��@IJ�@H�5@H��@H_@H7@H%�@G�F@F��@F�H@F6�@E�)@E��@Ex�@E7L@D�)@D��@DPH@C�}@C�*@Cn/@B��@B\�@BR�@A�Z@B@B�@A�@A�"@Ax�@@��@?�@@>�F@>q�@=�#@=�@=��@=�>@=�@<�e@<�@;�
@;n/@;�@:� @:�@9��@9(�@8�U@8PH@8~@7�
@7��@7n/@7@O@7�@6�s@6��@6@�@6J@5�#@5�@5�#@5�d@5�-@5��@5�n@5��@5��@5��@5��@5�@5�C@5L�@4��@4��@4U2@4	�@3��@3��@3�*@3�	@3j�@2�8@2�\@2Z�@23�@2�@1�.@1�@1��@1�@0�@0c�@/�@/l�@.�M@.�@.q�@.)�@-�@-��@-<6@-%F@-�@,�@,��@,M@+��@+��@+�V@+g�@+�@*��@*�@*~�@*1�@)ϫ@)��@)Y�@)V@(�[@(�@(��@(]d@(,=@(G@'��@'�f@'g�@'F�@'.I@'�@&�@&ߤ@&�R@&�A@&3�@%�@%��@%ԕ@%�z@%�@%L�@%@$��@$�j@$��@$(�@#��@#�@@#n/@#O@#"�@#�@"�6@"Ov@!�.@!�@!�-@!�h@!F@!5�@!0�@!�@ �j@ ��@ $@�m@��@l�@H�@�@҉@�F@c @�@_@�@��@�M@e,@L�@	l@��@�O@�@V�@�@��@خ@�@��@�[@�{@_p@U�@>�@�@�<@�\@z@q�@d�@\�@)�@�D@��@�@��@}�@�@�@Ɇ@�.@m�@bN@bN@bN@]d@9X@��@y�@A�@�@�1@�r@z@Q@�@�9@��@�d@�@f�@X@�@Ɇ@�$@��@��@�I@M@'R@M@@�@�r@�a@t�@W?@A�@,�@�@�@͟@��@��@p;@@�@
�@�@��@��@e,@7L@@�K@�@��@�.@K^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��>A̹�A�D�A��<Aˀ�A�K�A�.A��A�A�A��A���A��A�X�A�5A��Aƙ�A�7�A��Aű[AşVA�cA�gmA�M�A�_A�MA��.A���A��8A���AĽAİ!Aİ!AĵAĒ�A�49A��A�PA�0!A�,=A��QA�:^A��EA�/�A�W�A�W�A��QA���A��1A���A���A�A��A�GA�\�A��A�?�A�YA���A��eA��	A�XEA�rGA���A���A��A���A��A��A���A���A��/A���A� �A�d�A�c�A�9XA���A�I�A���A�gmA��A���A��nA�u�A�֡A���A��A|�!A{U2Aw2�As�MAo��An)_AmAjK^Ag�dAe�SA`@�A\4AX_AS��AM��AH��AEIRAD��AC�9AB��AB�AAj�AA�)A@{A?(A>�'A>	lA<��A<��A<J#A9RTA4qA2��A1S�A/X�A-&�A,�MA,�zA-��A/[�A.�sA-�kA+��A*�3A)��A' �A$�8A$�#A!eA�A�A��AD�A�AߤA�A�rA1�A�|A��A~�A�'A_A��Ay�AS&AC-A%A�AiDA5?A�zA�A}VAqA�#A��A�A�A��AC-A�A�A^5AU2A�AdZA]�A�sAbNA��A�A8A�\A��A�}A��A;�A�NA_A
�uA	]dA��AHA�As�A^5AFA7�AjA�AkQA;dA�A�'A�>A-�A cA `BA q@��F@�{�@�!-@��@��A@�S�@��@�H�@��]@�y>@��@���@��V@��$@�l�@�s@��@���@���@�m]@�B�@�q@�%F@�@�l"@�N�@��@�+@���@��@��m@��@쀝@�@�|�@�&�@�U�@�ی@�S&@���@�F@�@��]@藍@�)�@�S@�O�@�ں@�l"@�w2@���@�7@�}@�<�@�E9@�L@�8�@߳�@� i@���@���@��@އ+@�m�@��@ެ�@��@ީ�@�#:@�,�@��@ݐ�@ݖS@܋D@��&@�L0@ې�@�V�@ي	@�&�@��Q@�:*@ջ0@Վ�@�8@�s�@��@�z�@ָR@��/@��@�ی@�l�@Ӌ�@��	@үO@���@ҧ@���@�!�@�f�@��@Μx@Β�@�z�@��@�xl@�rG@�?}@��@���@ʒ�@��@�y>@�d�@��@�ݘ@�6z@��@���@ǀ4@�RT@�C@ƣ�@��&@ğ�@� �@Ý�@F@�E�@���@�E�@�qv@���@�:�@���@��@���@��s@�N<@�t�@�;d@�ں@�z�@���@��@���@�M@���@��_@�'R@��D@���@�@���@��u@�,=@��F@�Dg@��e@�e�@�M�@�>B@��#@�Z�@�5�@��@��/@��\@��9@���@�S@�~�@�@���@�zx@�RT@��@�l"@�1'@�	@�8�@��
@��$@�9�@��@���@��j@�q�@�+k@��@��]@��@���@�y�@��@��m@��+@�D�@�M@���@�s�@��@��#@�N<@�8@�IR@��E@�e�@�h
@�:*@��@�O�@�@�?}@��p@�M�@��@��A@���@���@�s@�.I@��@���@�l�@�:�@��@��@��9@��=@�F@�҉@��L@�m�@�D�@��@��w@���@��@�+@��`@�E�@���@�ϫ@��@��@�V@��@���@�o @���@�!@�y�@�q@��f@��@�4@��-@��@�c@�A @�o@��$@�~�@��A@��	@��U@�Q�@�e@���@�^�@�IR@�P�@�H�@�=�@�5�@�.I@�-w@�)_@�@���@���@�g8@���@�}�@�x�@��$@��h@�n/@�33@��@��`@���@�u%@�Ft@�	@��@��^@�e�@�+�@���@�tT@�($@��j@��S@�P�@��@��@��L@�q�@�	@��=@�e,@�\)@�2a@��5@�҉@��@��@���@��u@�q@�_@�+k@��@�s@�O�@�%F@��@���@�%�@��Q@�dZ@�A�@�!-@��@�]d@��)@��@��3@��@��4@�[W@���@���@�e�@� �@��@���@��H@���@�l�@�=�@���@���@�L0@��j@�e,@�O�@�)_@��|@���@�q�@�+k@��@��d@���@��g@���@�`B@�N<@�A�@�@��P@��6@���@�a|@�
@'�@~h
@}��@}�@}�@|A�@{�f@{>�@{�@z�r@z��@z��@z�<@z��@z�@yf�@x:�@w�g@w��@v�M@vȴ@v��@uIR@s�@st�@s�@rxl@q�@p�$@o�@n��@m��@m��@lM@k��@k�@j͟@j��@jZ�@jR�@j&�@i�j@j �@i��@i�M@h��@gj�@g�F@g�@@g�f@g�@gU�@f��@e��@e%F@de�@d:�@d�@c�W@b��@b0U@a��@a��@a;@`��@`m�@`M@`�@_x@^�y@^��@^O@]��@]o @\~(@\/�@\�@\S�@\9X@[��@[��@[�q@[v`@[P�@["�@[�@[y�@[H�@[o@Z�y@Z�\@Y��@Y�@Y�C@Y�^@Yzx@YQ�@Y:�@Yc�@YIR@X��@Xw�@X�.@X��@X�v@X��@Xu�@XD�@X!@X�@W��@Wb�@W�@V�@V��@U�@T:�@Sqv@SO@SP�@S;d@S�@S(@R�\@R@Q��@Qm]@Q \@P��@Pی@P�E@P��@O�
@OJ#@N��@NTa@Mp�@L�v@L�@LN�@K�@Kv`@J�8@Jp;@J-@I��@IJ�@H�5@H��@H_@H7@H%�@G�F@F��@F�H@F6�@E�)@E��@Ex�@E7L@D�)@D��@DPH@C�}@C�*@Cn/@B��@B\�@BR�@A�Z@B@B�@A�@A�"@Ax�@@��@?�@@>�F@>q�@=�#@=�@=��@=�>@=�@<�e@<�@;�
@;n/@;�@:� @:�@9��@9(�@8�U@8PH@8~@7�
@7��@7n/@7@O@7�@6�s@6��@6@�@6J@5�#@5�@5�#@5�d@5�-@5��@5�n@5��@5��@5��@5��@5�@5�C@5L�@4��@4��@4U2@4	�@3��@3��@3�*@3�	@3j�@2�8@2�\@2Z�@23�@2�@1�.@1�@1��@1�@0�@0c�@/�@/l�@.�M@.�@.q�@.)�@-�@-��@-<6@-%F@-�@,�@,��@,M@+��@+��@+�V@+g�@+�@*��@*�@*~�@*1�@)ϫ@)��@)Y�@)V@(�[@(�@(��@(]d@(,=@(G@'��@'�f@'g�@'F�@'.I@'�@&�@&ߤ@&�R@&�A@&3�@%�@%��@%ԕ@%�z@%�@%L�@%@$��@$�j@$��@$(�@#��@#�@@#n/@#O@#"�@#�@"�6@"Ov@!�.@!�@!�-@!�h@!F@!5�@!0�@!�@ �j@ ��@ $@�m@��@l�@H�@�@҉@�F@c @�@_@�@��@�M@e,@L�@	l@��@�O@�@V�@�@��@خ@�@��@�[@�{@_p@U�@>�@�@�<@�\@z@q�@d�@\�@)�@�D@��@�@��@}�@�@�@Ɇ@�.@m�@bN@bN@bN@]d@9X@��@y�@A�@�@�1@�r@z@Q@�@�9@��@�d@�@f�@X@�@Ɇ@�$@��@��@�I@M@'R@M@@�@�r@�a@t�@W?@A�@,�@�@�@͟@��@��@p;@@�@
�@�@��@��@e,@7L@@�K@�@��@�.@K^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�NB�4B�B�B�B�B��B�FB��B�B�B��B�B�HB	4B	D�B	D�B	>wB	1�B	*�B	'�B	(
B	)�B	)B	&�B	#�B	$B	$@B	$tB	#�B	!-B	"�B	$&B	+B	7�B	S�B	e�B	|PB
"�B
+�B
-�B
�JB
�XB
ڠB
�*B
�B
�	B�BB1�B>�BL0BD�BH�BW�BZ�B`�B_;BZ�BX�BVmBPbBLBK)BHfB?HB8�B'RB�B�B�B:B�B�B
�dB
�yB
�=B
�B
��B
}�B
E�B
�B
�B	�UB	�B	�2B	��B	��B	��B	��B	�9B	�2B	�XB	��B	��B	��B	~BB	raB	ffB	H�B	;B	3�B	�B��B��B��B��B��B�PB�sB��B�PB	?B	�B	�B	
#B		�B	�B	7LB	-�B	�B	 �B��B�kB�|B�B�cB	2�B	e,B	q'B	kkB	a-B	^�B	WYB	IB	9$B	>wB	~B�-B�=B�!B�IB�B�GB	 �B	?B	-]B	d�B	�SB	�@B	��B	�|B	�ZB	��B	�wB	�MB	�_B	ȚB	�rB	�gB	��B	��B	��B	�kB	ԕB	�7B	��B	�B	�4B	��B	��B	�B	��B	�(B	��B	��B	��B	��B	�bB	�EB	��B	��B	өB	�aB	�SB	��B	�aB	�HB	��B	��B	�}B	��B	��B	��B	��B	�}B	�GB	��B	��B	�1B	��B	�B	͹B	ʦB	�mB	�UB	��B	��B	��B	��B	��B	��B	�B	�!B	�$B	�
B	��B	�B	��B	��B	�wB	�OB	�[B	�[B	�B	�9B	��B	�LB	�2B	��B	�fB	��B	�2B	��B	��B	��B	��B	��B	�B	�;B	��B	āB	�B	��B	ĜB	�B	�B	ŢB	�zB	��B	�6B	͹B	�BB	��B	ϫB	��B	�VB	�JB	�RB	��B	ǔB	�gB	�aB	��B	��B	�mB	żB	�tB	�7B	��B	��B	ѝB	өB	רB	�B	�hB	�B	��B	��B	�bB	��B	��B	�CB	�qB	�B	�B	��B	�B	�B	��B	�vB	�vB	�B	�B	�ZB	��B	��B	��B	��B	�B	�wB	�B	�B	�B	�B	�B	��B	��B	�HB	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	�
B	�B	�QB	�B	�B	��B	�B	��B	��B	�"B	��B	�WB	��B	�B	�=B	��B	�RB	�B	�B	��B	��B	޸B	��B	��B	ܒB	ܒB	��B	�B	��B	�nB	��B	�B	�B	��B	�*B	�B	�B	�B	�B	��B	��B	�B	�AB	�B	��B	��B	�|B	��B	�MB	�B	�B	�B	�B	��B	�%B	�%B	�tB	�tB	�tB	��B	��B	��B	�B	�RB	��B	��B	��B	�>B	�DB	��B	�6B	�cB
 �B
 B
�B
�B
�B
aB
SB
�B
%B
�B
zB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
?B
�B
�B
�B
?B
mB
B
EB
%B
�B
B
zB
�B
�B
�B
?B
?B
?B
YB
�B
�B
�B
+B
�B
�B
fB
fB
�B
	7B
	�B

XB
^B
�B
dB
B
�B
�B
�B
�B
�B
dB
�B
�B
�B
�B
\B
�B
�B
B
"B
B
B
�B
"B
�B
�B
_B
yB
�B
+B
�B
�B
�B
�B
�B
�B
MB
�B
B
1B
�B
	B
=B
�B
�B
�B
�B
B
jB
jB
�B
�B
B
�B
 \B
 �B
!B
!-B
!|B
!�B
!�B
"B
!�B
"hB
"�B
"�B
"�B
#B
"�B
#TB
#nB
#nB
#�B
#TB
#�B
$tB
$�B
$�B
%B
%B
$�B
$�B
%B
%zB
%�B
%�B
%�B
&B
&fB
&�B
&�B
&�B
'B
'mB
'RB
'�B
'�B
(>B
(�B
(�B
*0B
*B
*B
+B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.cB
.cB
.B
.cB
.�B
.�B
/ B
/5B
/�B
0!B
0B
/�B
/ B
.�B
.}B
.B
.cB
.}B
/OB
.�B
/B
0oB
2aB
2�B
2�B
2�B
2�B
33B
3�B
3�B
3�B
4�B
4TB
4�B
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
6�B
8�B
8�B
9�B
9�B
:DB
9�B
:�B
:�B
:�B
:xB
:^B
8lB
6FB
6+B
8�B
7�B
6B
5B
4nB
2�B
3MB
5�B
5B
4B
4B
4�B
5?B
5�B
6+B
6`B
6�B
8�B
:B
:*B
:�B
8�B
9XB
9�B
:B
9�B
:�B
:xB
9$B
9�B
:B
9�B
9�B
:B
9�B
:DB
:*B
:B
:*B
9�B
9�B
9�B
9XB
9�B
8�B
9>B
9rB
:*B
:*B
;B
;0B
;�B
>�B
?HB
?.B
@B
@�B
@�B
@�B
@�B
AB
B�B
D3B
D3B
D3B
D�B
C�B
D�B
E9B
E�B
IRB
I�B
J�B
K�B
M�B
M�B
M�B
NB
NB
OBB
OvB
O\B
O(B
N�B
N�B
N�B
N�B
O�B
PB
O�B
P.B
N�B
NB
N<B
N"B
N<B
N<B
N"B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
O�B
PB
P�B
QB
Q�B
Q�B
RTB
R�B
R�B
SB
S@B
SuB
S�B
TB
TB
S�B
S�B
S�B
TaB
T�B
U�B
V9B
V�B
V�B
W�B
WsB
W?B
W$B
W
B
XB
W�B
XEB
XEB
X_B
X_B
XyB
X_B
X�B
X�B
Y�B
ZB
Z�B
[WB
^OB
]�B
]~B
[�B
Z�B
Z�B
ZQB
[WB
\)B
^5B
]�B
\�B
\�B
]~B
]IB
\�B
\]B
\�B
]/B
]dB
]�B
^�B
^�B
_!B
_pB
_VB
_�B
_�B
`'B
`BB
`�B
a-B
a�B
bNB
b�B
cTB
c�B
d�B
ezB
fB
gRB
gRB
g�B
g�B
h�B
iB
iyB
i�B
i�B
i�B
jB
j0B
jB
jKB
jKB
jB
kB
kB
kB
kB
kB
k6B
k6B
k�B
k�B
lB
lqB
l�B
l�B
m)B
mB
l�B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
ncB
n}B
n�B
n�B
oB
o�B
oOB
o5B
o�B
o�B
o�B
o�B
pUB
p!B
p!B
pUB
p;B
p�B
p�B
p�B
p�B
p�B
qB
p�B
q'B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
rB
r-B
raB
raB
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u�B
utB
u�B
u�B
vB
v+B
v+B
vzB
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
y$B
y$B
y�B
y�B
y�B
y�B
y�B
zB
y�B
z*B
zDB
z^B
zxB
z�B
z�B
{0B
{B
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
|6B
|jB
|�B
}B
}<B
}"B
}VB
}VB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~�B
~�B
~�B
~�B
~�B
B
cB
cB
}B
}B
}B
}B
�B
�4B
�OB
�iB
�iB
��B
��B
��B
�B
� B
� B
�UB
�oB
��B
��B
��B
�'B
�[B
�AB
��B
��B
��B
��B
�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�NB�4B�B�B�B�B��B�FB��B�B�B��B�B�HB	4B	D�B	D�B	>wB	1�B	*�B	'�B	(
B	)�B	)B	&�B	#�B	$B	$@B	$tB	#�B	!-B	"�B	$&B	+B	7�B	S�B	e�B	|PB
"�B
+�B
-�B
�JB
�XB
ڠB
�*B
�B
�	B�BB1�B>�BL0BD�BH�BW�BZ�B`�B_;BZ�BX�BVmBPbBLBK)BHfB?HB8�B'RB�B�B�B:B�B�B
�dB
�yB
�=B
�B
��B
}�B
E�B
�B
�B	�UB	�B	�2B	��B	��B	��B	��B	�9B	�2B	�XB	��B	��B	��B	~BB	raB	ffB	H�B	;B	3�B	�B��B��B��B��B��B�PB�sB��B�PB	?B	�B	�B	
#B		�B	�B	7LB	-�B	�B	 �B��B�kB�|B�B�cB	2�B	e,B	q'B	kkB	a-B	^�B	WYB	IB	9$B	>wB	~B�-B�=B�!B�IB�B�GB	 �B	?B	-]B	d�B	�SB	�@B	��B	�|B	�ZB	��B	�wB	�MB	�_B	ȚB	�rB	�gB	��B	��B	��B	�kB	ԕB	�7B	��B	�B	�4B	��B	��B	�B	��B	�(B	��B	��B	��B	��B	�bB	�EB	��B	��B	өB	�aB	�SB	��B	�aB	�HB	��B	��B	�}B	��B	��B	��B	��B	�}B	�GB	��B	��B	�1B	��B	�B	͹B	ʦB	�mB	�UB	��B	��B	��B	��B	��B	��B	�B	�!B	�$B	�
B	��B	�B	��B	��B	�wB	�OB	�[B	�[B	�B	�9B	��B	�LB	�2B	��B	�fB	��B	�2B	��B	��B	��B	��B	��B	�B	�;B	��B	āB	�B	��B	ĜB	�B	�B	ŢB	�zB	��B	�6B	͹B	�BB	��B	ϫB	��B	�VB	�JB	�RB	��B	ǔB	�gB	�aB	��B	��B	�mB	żB	�tB	�7B	��B	��B	ѝB	өB	רB	�B	�hB	�B	��B	��B	�bB	��B	��B	�CB	�qB	�B	�B	��B	�B	�B	��B	�vB	�vB	�B	�B	�ZB	��B	��B	��B	��B	�B	�wB	�B	�B	�B	�B	�B	��B	��B	�HB	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	�
B	�B	�QB	�B	�B	��B	�B	��B	��B	�"B	��B	�WB	��B	�B	�=B	��B	�RB	�B	�B	��B	��B	޸B	��B	��B	ܒB	ܒB	��B	�B	��B	�nB	��B	�B	�B	��B	�*B	�B	�B	�B	�B	��B	��B	�B	�AB	�B	��B	��B	�|B	��B	�MB	�B	�B	�B	�B	��B	�%B	�%B	�tB	�tB	�tB	��B	��B	��B	�B	�RB	��B	��B	��B	�>B	�DB	��B	�6B	�cB
 �B
 B
�B
�B
�B
aB
SB
�B
%B
�B
zB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
?B
�B
�B
�B
?B
mB
B
EB
%B
�B
B
zB
�B
�B
�B
?B
?B
?B
YB
�B
�B
�B
+B
�B
�B
fB
fB
�B
	7B
	�B

XB
^B
�B
dB
B
�B
�B
�B
�B
�B
dB
�B
�B
�B
�B
\B
�B
�B
B
"B
B
B
�B
"B
�B
�B
_B
yB
�B
+B
�B
�B
�B
�B
�B
�B
MB
�B
B
1B
�B
	B
=B
�B
�B
�B
�B
B
jB
jB
�B
�B
B
�B
 \B
 �B
!B
!-B
!|B
!�B
!�B
"B
!�B
"hB
"�B
"�B
"�B
#B
"�B
#TB
#nB
#nB
#�B
#TB
#�B
$tB
$�B
$�B
%B
%B
$�B
$�B
%B
%zB
%�B
%�B
%�B
&B
&fB
&�B
&�B
&�B
'B
'mB
'RB
'�B
'�B
(>B
(�B
(�B
*0B
*B
*B
+B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.cB
.cB
.B
.cB
.�B
.�B
/ B
/5B
/�B
0!B
0B
/�B
/ B
.�B
.}B
.B
.cB
.}B
/OB
.�B
/B
0oB
2aB
2�B
2�B
2�B
2�B
33B
3�B
3�B
3�B
4�B
4TB
4�B
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
6�B
8�B
8�B
9�B
9�B
:DB
9�B
:�B
:�B
:�B
:xB
:^B
8lB
6FB
6+B
8�B
7�B
6B
5B
4nB
2�B
3MB
5�B
5B
4B
4B
4�B
5?B
5�B
6+B
6`B
6�B
8�B
:B
:*B
:�B
8�B
9XB
9�B
:B
9�B
:�B
:xB
9$B
9�B
:B
9�B
9�B
:B
9�B
:DB
:*B
:B
:*B
9�B
9�B
9�B
9XB
9�B
8�B
9>B
9rB
:*B
:*B
;B
;0B
;�B
>�B
?HB
?.B
@B
@�B
@�B
@�B
@�B
AB
B�B
D3B
D3B
D3B
D�B
C�B
D�B
E9B
E�B
IRB
I�B
J�B
K�B
M�B
M�B
M�B
NB
NB
OBB
OvB
O\B
O(B
N�B
N�B
N�B
N�B
O�B
PB
O�B
P.B
N�B
NB
N<B
N"B
N<B
N<B
N"B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
O�B
PB
P�B
QB
Q�B
Q�B
RTB
R�B
R�B
SB
S@B
SuB
S�B
TB
TB
S�B
S�B
S�B
TaB
T�B
U�B
V9B
V�B
V�B
W�B
WsB
W?B
W$B
W
B
XB
W�B
XEB
XEB
X_B
X_B
XyB
X_B
X�B
X�B
Y�B
ZB
Z�B
[WB
^OB
]�B
]~B
[�B
Z�B
Z�B
ZQB
[WB
\)B
^5B
]�B
\�B
\�B
]~B
]IB
\�B
\]B
\�B
]/B
]dB
]�B
^�B
^�B
_!B
_pB
_VB
_�B
_�B
`'B
`BB
`�B
a-B
a�B
bNB
b�B
cTB
c�B
d�B
ezB
fB
gRB
gRB
g�B
g�B
h�B
iB
iyB
i�B
i�B
i�B
jB
j0B
jB
jKB
jKB
jB
kB
kB
kB
kB
kB
k6B
k6B
k�B
k�B
lB
lqB
l�B
l�B
m)B
mB
l�B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
ncB
n}B
n�B
n�B
oB
o�B
oOB
o5B
o�B
o�B
o�B
o�B
pUB
p!B
p!B
pUB
p;B
p�B
p�B
p�B
p�B
p�B
qB
p�B
q'B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
rB
r-B
raB
raB
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u�B
utB
u�B
u�B
vB
v+B
v+B
vzB
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
y$B
y$B
y�B
y�B
y�B
y�B
y�B
zB
y�B
z*B
zDB
z^B
zxB
z�B
z�B
{0B
{B
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
|6B
|jB
|�B
}B
}<B
}"B
}VB
}VB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~�B
~�B
~�B
~�B
~�B
B
cB
cB
}B
}B
}B
}B
�B
�4B
�OB
�iB
�iB
��B
��B
��B
�B
� B
� B
�UB
�oB
��B
��B
��B
�'B
�[B
�AB
��B
��B
��B
��B
�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230320064602  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230320064609  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230320064614  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230320064615                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230320064618  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230320064618  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230320070027                      G�O�G�O�G�O�                