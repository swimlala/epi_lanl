CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:16:08Z creation;2022-06-04T19:16:08Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191608  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��T��71   @��U'@�t@-z^5?|��c�t�j~�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BG��BN  BX  B`  Bh  BpffBx  B33B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C�fC�fC  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dރ3D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @
=q@p��@�Q�@�Q�A(�A<(�A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
=B
=B
=B'
=B/
=B7p�B?
=BF��BM
=BW
=B_
=Bg
=Bop�Bw
=B~=pB��B��B��B��RB��B�Q�B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B�Q�CC��C��CC	CCCCCCCCCCC�)C!�)C#C%��C'��C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCY�)C[�)C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIw
DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�;�D�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD���D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�;�D�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�{�D޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�a�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�!bA�"hA�'RA�($A�(XA�'�A�*0A�)*A�'�A�'A�(�A�*�A�1�A�4�A�3hA�4�A�2-A�/A�-�A�-wA�(�A�#:A�~A��.Aٺ*A�T�AءbA� �A��AҴ9A��	A�VA�7AŢ�AĦLA�՛A�/�A���A���A�خA���A�J�A���A��A���A�4A��A�gA�}VA��A���A��A��<A��A�6�A��A�ŢA��"A�>�A�*eA�f�A���A�1'A���A�~�A�v�A��9A��)A��?A��A��A��,A�{A���A�@�A��FA�ߤA�kQA�9�A�`vA�Aw��ArAj�AbiDA`Z�A`9�A]�!A[zASqvAOXAHt�AF;dAE �AA��A@��A?��A>� A=	�A;�4A:��A8��A6�RA6'�A4��A4E9A3  A1�,A/��A,�A,I�A)  A'��A%kQA#dZA!�A R�A��Au�A��A-AخA��A?A��AZ�A5?A:�AP�AjA~A~�A��A��A��A�A�XA��A��A�A�A�rA��A��AXyA��A+�A@�A��A�6AJ�A\�AVA�XA
�A%AƨA��A
��A
�A	7A��A	A	zA	��A	�aA
	�A
�4A
�A
�.A	qvA�AN�AC�A��A�Al�A�	A�'A:*A��Au%A9XA�7A�?A��A>BA��A��A�)A��A($AC�A�A �A �k@��@��@��@�O�@�҉@�H�@�N<@�q@�R�@��@��m@�}�@�� @��@��$@���@���@�\�@�k�@���@���@�0U@��@�R�@���@��@�C�@��]@�@�@���@���@�Z�@�5�@�+k@��@�m]@��@�5�@�q@���@�@���@���@扠@�F@��@��@�,=@�@��D@�$@�@ᧇ@�ƨ@�a�@�'R@���@�L0@�u%@�{@�@��m@��@�y>@ۥ@�@�@�-@ڦL@ڶ�@���@�C-@�qv@��@�D�@؉�@؃�@ד@� i@��@��K@�-@�\�@���@ә�@���@�e�@��@Ѽ�@�e�@Ж�@�7�@ϸ�@�A @��@�D�@̐.@�� @��z@˛=@�S@��'@ʿ�@��j@�o�@�S�@��@�%@ȸR@� �@ǵt@�O@Ɗr@��@���@�iD@��@��)@�6z@���@�	�@���@��@@�s@�%F@��<@��@�oi@��@���@��~@��@��Y@�H@���@�L�@��@�ی@�� @�Ov@��}@�\�@��"@���@��@��}@�m�@���@�[W@��@��1@�Q@�@��N@�b�@��@��6@�kQ@��r@��{@�Dg@�%@���@�D�@��z@�[W@�@��A@�6�@���@�)_@�S@���@�� @�;�@��@�33@��<@�N�@��;@���@�a�@���@�9X@��@���@�Vm@�@@��O@�1'@�خ@��=@�G�@�+�@��@��b@�@�@��@�}�@��@�Ĝ@���@�Ov@�$@���@���@�%@�\�@��r@���@���@�X�@�	l@���@�h�@�9X@��@���@�'�@��m@�Z�@�+k@�$@��@�
�@���@�a�@��X@�:*@���@�E9@��@��$@�\�@��@��Q@���@�+@���@���@���@�Ta@�($@�خ@�n/@��5@��@�M�@�)�@��Z@���@�s�@�/�@���@�҉@��@��o@���@��@@�=�@�ߤ@��p@��B@��R@�PH@��@���@�ϫ@��@���@�a@���@���@�z@�Z�@�;�@�@��@���@�0�@��X@���@��A@�j@�1'@���@��{@�7L@��@��@��@��4@�q�@�YK@�@��A@���@��@���@�!�@���@�ѷ@��u@�PH@��@��@���@�b�@�&@�
=@���@��o@�0U@��A@���@��@�ѷ@�z@�2�@�1@��a@�G�@�C@�ߤ@���@���@��@���@���@�e�@�K�@�/�@��@��@��@�{J@�Vm@�!-@��@�d�@�$@���@���@���@�n/@�O�@�IR@��@���@��.@�`�@�Ov@�/�@�W@��@��@�f@e�@~W�@}�@|c�@|_@{�@{X�@zkQ@y�@y�@yhs@y7L@y-w@y4@x�p@x"h@xz�@xS�@w��@v+k@u��@u�M@u�"@up�@uIR@uIR@u^�@ux�@tD�@sRT@s'�@s)_@rn�@q�>@p��@p��@pS�@p1'@o��@o>�@n�@n�@nh
@n@m�h@l��@l��@l��@lPH@kخ@ks@j��@j�+@jTa@j�@i��@i`B@i�@h��@h�@hI�@g��@gj�@g8@g(@f��@f{�@e�@ex�@e*0@e%@d��@dbN@dM@c�@c6z@b�R@bc @a��@aϫ@a�t@a��@aN<@a�@`�E@`Xy@`G@_˒@_y�@_4�@_@^ȴ@^d�@^�@]�@]�N@]��@]^�@]%F@]*0@\��@\x@[��@[E9@Z�H@Zv�@Z$�@Z@Y��@Y�^@Yf�@Y?}@Y	l@X�D@XD�@XA�@W�@V�8@V�@V�\@Vl�@V#:@U��@U�7@U=�@T�O@T<�@T  @S�a@S��@SMj@S�@R�L@RV@R#:@R@Q�9@Q��@Qw2@Q=�@Q@@P��@P�j@PQ�@O�Q@O��@OS�@O�@N�\@Nff@M�@MJ�@L�U@L�@Ko�@JR�@Iϫ@I�t@I�C@I��@Ic�@I!�@HtT@H<�@G�&@G�V@GA�@F�c@F�m@F��@F�+@Fh
@E�^@E�@D�?@Dg8@C�@C;d@B�@BV@A�#@A[W@A�@@�@@��@@�z@@�@@bN@?�@?� @?�V@?!-@>�B@>��@>ff@>:*@=��@=O�@<�@<�@<M@;� @;�k@;l�@;�@:�<@:�@9@9J�@9�@8�|@8�E@8�$@8g8@8A�@8�@7��@7�f@7\)@7�@6��@6kQ@63�@6�@5��@5��@5^�@5	l@4��@4�I@4r�@3ݘ@3y�@3,�@3o@2��@2�]@2�R@2�+@2Ov@1�>@1X@1�@0�j@0��@0��@0��@0r�@0b@/�@/��@/�f@/=@/Y@.�@.��@.s�@.�@-��@-�S@-F@-%F@,֡@,��@,tT@+��@+|�@+n/@+)_@*��@*͟@*�m@*��@*&�@)��@)��@)�=@)?}@(�@(�.@(w�@(`�@(A�@(  @'�0@'��@'a@'C�@'+@'!-@&��@&��@&Q@&!�@%�@%�"@%s�@%f�@%c�@%S&@%�@$�@$�.@$g8@$1'@#�r@#��@#��@#e�@#$t@"��@"�\@"L0@"6�@!�@!��@![W@ �v@ �o@ Q�@ <�@ $@�+@خ@�0@�:@8@�@�6@^5@!�@��@`B@8�@�@��@��@~(@$@��@��@qv@H�@,�@C@�c@��@a|@M�@5?@�D@�t@�"@s�@!�@%@�P@�f@��@��@:�@��@� @��@$t@��@�8@҉@}V@d�@Ta@Ov@#:@@�@��@zx@L�@��@�.@bN@�@�@�;@��@j�@P�@�@��@h
@Q@-@�@��@�S@Y�@�@�v@��@��@m�@I�@	�@��@�@��@�{@RT@�@͟@��@�r@?@�@ԕ@��@c�@N<@:�@;@�U@��@��@�z@�D@u�@S�@�@�@خ@�w@��@��@��@C�@9�@8@.I@�@@
��@
�]@
�m@
��@
z@
@�@
�@	��@	�@	��@	w2@	N<@	7L@	+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�!bA�"hA�'RA�($A�(XA�'�A�*0A�)*A�'�A�'A�(�A�*�A�1�A�4�A�3hA�4�A�2-A�/A�-�A�-wA�(�A�#:A�~A��.Aٺ*A�T�AءbA� �A��AҴ9A��	A�VA�7AŢ�AĦLA�՛A�/�A���A���A�خA���A�J�A���A��A���A�4A��A�gA�}VA��A���A��A��<A��A�6�A��A�ŢA��"A�>�A�*eA�f�A���A�1'A���A�~�A�v�A��9A��)A��?A��A��A��,A�{A���A�@�A��FA�ߤA�kQA�9�A�`vA�Aw��ArAj�AbiDA`Z�A`9�A]�!A[zASqvAOXAHt�AF;dAE �AA��A@��A?��A>� A=	�A;�4A:��A8��A6�RA6'�A4��A4E9A3  A1�,A/��A,�A,I�A)  A'��A%kQA#dZA!�A R�A��Au�A��A-AخA��A?A��AZ�A5?A:�AP�AjA~A~�A��A��A��A�A�XA��A��A�A�A�rA��A��AXyA��A+�A@�A��A�6AJ�A\�AVA�XA
�A%AƨA��A
��A
�A	7A��A	A	zA	��A	�aA
	�A
�4A
�A
�.A	qvA�AN�AC�A��A�Al�A�	A�'A:*A��Au%A9XA�7A�?A��A>BA��A��A�)A��A($AC�A�A �A �k@��@��@��@�O�@�҉@�H�@�N<@�q@�R�@��@��m@�}�@�� @��@��$@���@���@�\�@�k�@���@���@�0U@��@�R�@���@��@�C�@��]@�@�@���@���@�Z�@�5�@�+k@��@�m]@��@�5�@�q@���@�@���@���@扠@�F@��@��@�,=@�@��D@�$@�@ᧇ@�ƨ@�a�@�'R@���@�L0@�u%@�{@�@��m@��@�y>@ۥ@�@�@�-@ڦL@ڶ�@���@�C-@�qv@��@�D�@؉�@؃�@ד@� i@��@��K@�-@�\�@���@ә�@���@�e�@��@Ѽ�@�e�@Ж�@�7�@ϸ�@�A @��@�D�@̐.@�� @��z@˛=@�S@��'@ʿ�@��j@�o�@�S�@��@�%@ȸR@� �@ǵt@�O@Ɗr@��@���@�iD@��@��)@�6z@���@�	�@���@��@@�s@�%F@��<@��@�oi@��@���@��~@��@��Y@�H@���@�L�@��@�ی@�� @�Ov@��}@�\�@��"@���@��@��}@�m�@���@�[W@��@��1@�Q@�@��N@�b�@��@��6@�kQ@��r@��{@�Dg@�%@���@�D�@��z@�[W@�@��A@�6�@���@�)_@�S@���@�� @�;�@��@�33@��<@�N�@��;@���@�a�@���@�9X@��@���@�Vm@�@@��O@�1'@�خ@��=@�G�@�+�@��@��b@�@�@��@�}�@��@�Ĝ@���@�Ov@�$@���@���@�%@�\�@��r@���@���@�X�@�	l@���@�h�@�9X@��@���@�'�@��m@�Z�@�+k@�$@��@�
�@���@�a�@��X@�:*@���@�E9@��@��$@�\�@��@��Q@���@�+@���@���@���@�Ta@�($@�خ@�n/@��5@��@�M�@�)�@��Z@���@�s�@�/�@���@�҉@��@��o@���@��@@�=�@�ߤ@��p@��B@��R@�PH@��@���@�ϫ@��@���@�a@���@���@�z@�Z�@�;�@�@��@���@�0�@��X@���@��A@�j@�1'@���@��{@�7L@��@��@��@��4@�q�@�YK@�@��A@���@��@���@�!�@���@�ѷ@��u@�PH@��@��@���@�b�@�&@�
=@���@��o@�0U@��A@���@��@�ѷ@�z@�2�@�1@��a@�G�@�C@�ߤ@���@���@��@���@���@�e�@�K�@�/�@��@��@��@�{J@�Vm@�!-@��@�d�@�$@���@���@���@�n/@�O�@�IR@��@���@��.@�`�@�Ov@�/�@�W@��@��@�f@e�@~W�@}�@|c�@|_@{�@{X�@zkQ@y�@y�@yhs@y7L@y-w@y4@x�p@x"h@xz�@xS�@w��@v+k@u��@u�M@u�"@up�@uIR@uIR@u^�@ux�@tD�@sRT@s'�@s)_@rn�@q�>@p��@p��@pS�@p1'@o��@o>�@n�@n�@nh
@n@m�h@l��@l��@l��@lPH@kخ@ks@j��@j�+@jTa@j�@i��@i`B@i�@h��@h�@hI�@g��@gj�@g8@g(@f��@f{�@e�@ex�@e*0@e%@d��@dbN@dM@c�@c6z@b�R@bc @a��@aϫ@a�t@a��@aN<@a�@`�E@`Xy@`G@_˒@_y�@_4�@_@^ȴ@^d�@^�@]�@]�N@]��@]^�@]%F@]*0@\��@\x@[��@[E9@Z�H@Zv�@Z$�@Z@Y��@Y�^@Yf�@Y?}@Y	l@X�D@XD�@XA�@W�@V�8@V�@V�\@Vl�@V#:@U��@U�7@U=�@T�O@T<�@T  @S�a@S��@SMj@S�@R�L@RV@R#:@R@Q�9@Q��@Qw2@Q=�@Q@@P��@P�j@PQ�@O�Q@O��@OS�@O�@N�\@Nff@M�@MJ�@L�U@L�@Ko�@JR�@Iϫ@I�t@I�C@I��@Ic�@I!�@HtT@H<�@G�&@G�V@GA�@F�c@F�m@F��@F�+@Fh
@E�^@E�@D�?@Dg8@C�@C;d@B�@BV@A�#@A[W@A�@@�@@��@@�z@@�@@bN@?�@?� @?�V@?!-@>�B@>��@>ff@>:*@=��@=O�@<�@<�@<M@;� @;�k@;l�@;�@:�<@:�@9@9J�@9�@8�|@8�E@8�$@8g8@8A�@8�@7��@7�f@7\)@7�@6��@6kQ@63�@6�@5��@5��@5^�@5	l@4��@4�I@4r�@3ݘ@3y�@3,�@3o@2��@2�]@2�R@2�+@2Ov@1�>@1X@1�@0�j@0��@0��@0��@0r�@0b@/�@/��@/�f@/=@/Y@.�@.��@.s�@.�@-��@-�S@-F@-%F@,֡@,��@,tT@+��@+|�@+n/@+)_@*��@*͟@*�m@*��@*&�@)��@)��@)�=@)?}@(�@(�.@(w�@(`�@(A�@(  @'�0@'��@'a@'C�@'+@'!-@&��@&��@&Q@&!�@%�@%�"@%s�@%f�@%c�@%S&@%�@$�@$�.@$g8@$1'@#�r@#��@#��@#e�@#$t@"��@"�\@"L0@"6�@!�@!��@![W@ �v@ �o@ Q�@ <�@ $@�+@خ@�0@�:@8@�@�6@^5@!�@��@`B@8�@�@��@��@~(@$@��@��@qv@H�@,�@C@�c@��@a|@M�@5?@�D@�t@�"@s�@!�@%@�P@�f@��@��@:�@��@� @��@$t@��@�8@҉@}V@d�@Ta@Ov@#:@@�@��@zx@L�@��@�.@bN@�@�@�;@��@j�@P�@�@��@h
@Q@-@�@��@�S@Y�@�@�v@��@��@m�@I�@	�@��@�@��@�{@RT@�@͟@��@�r@?@�@ԕ@��@c�@N<@:�@;@�U@��@��@�z@�D@u�@S�@�@�@خ@�w@��@��@��@C�@9�@8@.I@�@@
��@
�]@
�m@
��@
z@
@�@
�@	��@	�@	��@	w2@	N<@	7L@	+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�XB�XB�B�sB�B��B��B�B�B�$B�XB�B�B�B�B�B�)B�]B��B�'B��B��B��B�BB	%B	�B	$ZB	T�B	��B
'�B
/ B
7fB
5B
B�B
R�B
d�B
~B
�LB
�[B
�*B
��B B�B1B�B�B?�BF?Ba|Ba�Bg�BuBm)BlBq'Bl�B`vB`\BD�B=�B7�B'�BEB_B
�B
�hB
�VB�B�B
�BB
�B
��B
��B
��B
qAB
[#B
N�B
5tB
&�B
A�B
$�B	�yB	�B	lB	)�B	%�B	YB	\]B	LJB	%B	{B��B�B�gB�B��B�#B�B�AB��B��B��B�<B�B�$B�0B��B��B��B�KB�fB�OB�$B��B�xB�?B��B�B~�B��B��B�B�2B��B��B��B��B�-B�^B�]B��B�B�B��B�OB�B�B	�B	"hB	C{B	dtB	l�B	m�B	oOB	q�B	n}B	h�B	bB	S�B	K�B	?�B	72B	1�B	*B	$�B	 �B	�B	B	qB	~B	=B	B	+�B	;�B	FYB	O�B	Y�B	p!B	x8B	wB	r�B	m�B	k6B	g�B	aB	\�B	X_B	T�B	T�B	R�B	R�B	S�B	UMB	\�B	fB	g�B	g�B	f�B	gB	n�B	x�B	|6B	|�B	|�B	}�B	|jB	xB	v�B	z�B	}<B	|�B	|�B	z^B	{0B	�aB	�iB	}<B	�6B	�wB	��B	�yB	�&B	�cB	�UB	��B	��B	��B	�UB	�B	��B	�HB	��B	�pB	��B	�tB	��B	��B	��B	��B	��B	��B	�B	��B	�pB	�HB	�B	��B	��B	�qB	��B	��B	��B	��B	�B	�B	��B	��B	�dB	��B	�rB	��B	��B	�cB	��B	�rB	�XB	�B	�rB	ϑB	бB	�B	�B	̳B	҉B	�B	��B	�[B	�}B	�BB	уB	��B	��B	�sB	רB	��B	��B	��B	�:B	ӏB	�kB	�WB	�=B	�/B	�jB	��B	��B	�-B	�HB	�BB	�pB	�B	��B	�]B	��B	��B	��B	�/B	߾B	�;B	��B	�B	�B	�
B	�sB	��B	�B	�2B	��B	�B	�DB	�_B	�B	�LB	�B	� B	��B	��B	�B	�B	�B	�0B	�eB	��B	�B	�6B	�B	�B	�B	�B	��B	�B	�qB	�"B	�=B	��B	�B	�qB	�B	�B	�)B	�CB	�B	�cB	�B	��B	� B	� B	�B	��B	�B	��B	�vB	�B	��B	��B	�B	��B	�MB	�B	��B	��B	�B	��B	��B	��B	�2B	��B	��B	��B	�2B	��B	�B	�LB	�2B	�LB	�B	��B	��B	�ZB	�ZB	�tB	�B	�B	��B	�B	��B	�$B	�B	�B	�xB	�dB	�B	�B	�dB	�B	��B	�"B	��B	�]B	�B	�B	�.B	��B
 �B
 B
UB
�B
�B
AB
-B
�B
�B
�B
�B
%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
?B
�B
B
�B
EB
1B
�B
�B
	�B

�B
)B
xB
B
JB
�B
B
B
pB
�B
�B
�B
B
\B
�B
�B
�B
�B
�B
.B
B
�B
�B
�B
�B
vB
�B
hB
�B
�B
�B
�B
4B
�B
�B
�B
�B
uB
,B
�B
�B
B
�B
mB
�B
�B
�B
�B
�B
_B
yB
�B
�B
B
eB
KB
�B
QB
QB
kB
�B
�B
�B
�B
B
CB
�B
�B
�B
dB
~B
dB
�B
jB
�B
B
�B
pB
!B
�B
B
�B
B
�B
/B
�B
;B
 vB
!bB
 �B
!B
!B
 �B
!-B
!|B
"�B
!�B
!�B
!�B
!bB
!�B
#TB
#:B
"�B
# B
#TB
$�B
%�B
%�B
%�B
&fB
'mB
'B
'B
'B
'�B
(>B
(�B
(�B
)B
)_B
(�B
(
B
'�B
'�B
&�B
'mB
'�B
(XB
(�B
)�B
)�B
*B
,WB
,�B
.�B
/OB
.�B
-�B
-�B
.�B
/�B
1'B
1[B
1AB
1[B
1�B
1�B
0�B
1�B
2B
2�B
3�B
3�B
4�B
6FB
6�B
6�B
6�B
6�B
6�B
72B
8B
7�B
8RB
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:DB
:xB
;B
;dB
;�B
<B
<�B
<�B
<�B
=<B
=VB
=qB
=�B
>]B
>]B
>�B
>�B
?.B
?HB
?cB
?cB
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
B�B
CB
CaB
CB
B�B
BuB
BuB
C�B
D3B
DMB
DMB
D�B
D�B
EB
EmB
E�B
E�B
E�B
FB
F?B
FtB
F�B
F�B
F�B
FtB
F?B
F?B
F�B
F�B
F�B
GB
F�B
G+B
GzB
GzB
G�B
HB
H1B
HfB
H�B
H�B
H�B
IB
I7B
I�B
I�B
I�B
I�B
J	B
J#B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
LB
L0B
L~B
LJB
LdB
L�B
M6B
MjB
M�B
OB
OvB
O�B
O�B
O�B
O�B
O�B
P}B
PbB
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
QhB
Q�B
R B
RB
R:B
RoB
R�B
S[B
SuB
S�B
TFB
T�B
T�B
T�B
T�B
UMB
U2B
U�B
T�B
U�B
VB
VB
V9B
VSB
V�B
V�B
WYB
W?B
W$B
W�B
W�B
W�B
XB
XEB
XyB
X�B
Y1B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
Z7B
Z7B
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
[�B
[�B
\)B
\]B
\]B
\xB
\�B
]/B
]~B
]~B
]~B
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
_B
_B
^�B
^�B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`\B
`�B
`�B
`�B
aHB
a-B
a|B
a�B
abB
bB
bNB
bNB
b�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
cnB
c�B
d@B
d�B
d�B
d�B
d�B
d�B
e,B
eFB
ezB
ezB
ezB
ezB
e�B
e�B
fB
fB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
h�B
i*B
i_B
i*B
i�B
i�B
i�B
jeB
j�B
kB
kB
kB
k6B
k6B
kQB
kQB
k�B
k�B
k�B
lWB
lWB
l�B
m)B
m)B
mCB
m�B
m�B
m�B
n/B
n}B
ncB
n�B
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
p!B
p;B
p;B
p�B
p�B
p�B
p�B
q'B
q'B
qvB
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
sB
sB
sB
s3B
s3B
shB
s�B
s�B
s�B
tnB
tnB
t�B
t�B
t�B
t�B
u?B
utB
uZB
u�B
vFB
v`B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
xB
x8B
xRB
xlB
x�B
x�B
y$B
y$B
yXB
y�B
y�B
zxB
z^B
z�B
z�B
{B
{JB
{�B
{�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}<B
}�B
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
~]B
~wB
~�B
~�B
~�B
~�B
HB
B
.B
HB
.B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�XB�XB�B�sB�B��B��B�B�B�$B�XB�B�B�B�B�B�)B�]B��B�'B��B��B��B�BB	%B	�B	$ZB	T�B	��B
'�B
/ B
7fB
5B
B�B
R�B
d�B
~B
�LB
�[B
�*B
��B B�B1B�B�B?�BF?Ba|Ba�Bg�BuBm)BlBq'Bl�B`vB`\BD�B=�B7�B'�BEB_B
�B
�hB
�VB�B�B
�BB
�B
��B
��B
��B
qAB
[#B
N�B
5tB
&�B
A�B
$�B	�yB	�B	lB	)�B	%�B	YB	\]B	LJB	%B	{B��B�B�gB�B��B�#B�B�AB��B��B��B�<B�B�$B�0B��B��B��B�KB�fB�OB�$B��B�xB�?B��B�B~�B��B��B�B�2B��B��B��B��B�-B�^B�]B��B�B�B��B�OB�B�B	�B	"hB	C{B	dtB	l�B	m�B	oOB	q�B	n}B	h�B	bB	S�B	K�B	?�B	72B	1�B	*B	$�B	 �B	�B	B	qB	~B	=B	B	+�B	;�B	FYB	O�B	Y�B	p!B	x8B	wB	r�B	m�B	k6B	g�B	aB	\�B	X_B	T�B	T�B	R�B	R�B	S�B	UMB	\�B	fB	g�B	g�B	f�B	gB	n�B	x�B	|6B	|�B	|�B	}�B	|jB	xB	v�B	z�B	}<B	|�B	|�B	z^B	{0B	�aB	�iB	}<B	�6B	�wB	��B	�yB	�&B	�cB	�UB	��B	��B	��B	�UB	�B	��B	�HB	��B	�pB	��B	�tB	��B	��B	��B	��B	��B	��B	�B	��B	�pB	�HB	�B	��B	��B	�qB	��B	��B	��B	��B	�B	�B	��B	��B	�dB	��B	�rB	��B	��B	�cB	��B	�rB	�XB	�B	�rB	ϑB	бB	�B	�B	̳B	҉B	�B	��B	�[B	�}B	�BB	уB	��B	��B	�sB	רB	��B	��B	��B	�:B	ӏB	�kB	�WB	�=B	�/B	�jB	��B	��B	�-B	�HB	�BB	�pB	�B	��B	�]B	��B	��B	��B	�/B	߾B	�;B	��B	�B	�B	�
B	�sB	��B	�B	�2B	��B	�B	�DB	�_B	�B	�LB	�B	� B	��B	��B	�B	�B	�B	�0B	�eB	��B	�B	�6B	�B	�B	�B	�B	��B	�B	�qB	�"B	�=B	��B	�B	�qB	�B	�B	�)B	�CB	�B	�cB	�B	��B	� B	� B	�B	��B	�B	��B	�vB	�B	��B	��B	�B	��B	�MB	�B	��B	��B	�B	��B	��B	��B	�2B	��B	��B	��B	�2B	��B	�B	�LB	�2B	�LB	�B	��B	��B	�ZB	�ZB	�tB	�B	�B	��B	�B	��B	�$B	�B	�B	�xB	�dB	�B	�B	�dB	�B	��B	�"B	��B	�]B	�B	�B	�.B	��B
 �B
 B
UB
�B
�B
AB
-B
�B
�B
�B
�B
%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
?B
�B
B
�B
EB
1B
�B
�B
	�B

�B
)B
xB
B
JB
�B
B
B
pB
�B
�B
�B
B
\B
�B
�B
�B
�B
�B
.B
B
�B
�B
�B
�B
vB
�B
hB
�B
�B
�B
�B
4B
�B
�B
�B
�B
uB
,B
�B
�B
B
�B
mB
�B
�B
�B
�B
�B
_B
yB
�B
�B
B
eB
KB
�B
QB
QB
kB
�B
�B
�B
�B
B
CB
�B
�B
�B
dB
~B
dB
�B
jB
�B
B
�B
pB
!B
�B
B
�B
B
�B
/B
�B
;B
 vB
!bB
 �B
!B
!B
 �B
!-B
!|B
"�B
!�B
!�B
!�B
!bB
!�B
#TB
#:B
"�B
# B
#TB
$�B
%�B
%�B
%�B
&fB
'mB
'B
'B
'B
'�B
(>B
(�B
(�B
)B
)_B
(�B
(
B
'�B
'�B
&�B
'mB
'�B
(XB
(�B
)�B
)�B
*B
,WB
,�B
.�B
/OB
.�B
-�B
-�B
.�B
/�B
1'B
1[B
1AB
1[B
1�B
1�B
0�B
1�B
2B
2�B
3�B
3�B
4�B
6FB
6�B
6�B
6�B
6�B
6�B
72B
8B
7�B
8RB
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:DB
:xB
;B
;dB
;�B
<B
<�B
<�B
<�B
=<B
=VB
=qB
=�B
>]B
>]B
>�B
>�B
?.B
?HB
?cB
?cB
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
B�B
CB
CaB
CB
B�B
BuB
BuB
C�B
D3B
DMB
DMB
D�B
D�B
EB
EmB
E�B
E�B
E�B
FB
F?B
FtB
F�B
F�B
F�B
FtB
F?B
F?B
F�B
F�B
F�B
GB
F�B
G+B
GzB
GzB
G�B
HB
H1B
HfB
H�B
H�B
H�B
IB
I7B
I�B
I�B
I�B
I�B
J	B
J#B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
LB
L0B
L~B
LJB
LdB
L�B
M6B
MjB
M�B
OB
OvB
O�B
O�B
O�B
O�B
O�B
P}B
PbB
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
QhB
Q�B
R B
RB
R:B
RoB
R�B
S[B
SuB
S�B
TFB
T�B
T�B
T�B
T�B
UMB
U2B
U�B
T�B
U�B
VB
VB
V9B
VSB
V�B
V�B
WYB
W?B
W$B
W�B
W�B
W�B
XB
XEB
XyB
X�B
Y1B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
Z7B
Z7B
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
[�B
[�B
\)B
\]B
\]B
\xB
\�B
]/B
]~B
]~B
]~B
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
_B
_B
^�B
^�B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`\B
`�B
`�B
`�B
aHB
a-B
a|B
a�B
abB
bB
bNB
bNB
b�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
cnB
c�B
d@B
d�B
d�B
d�B
d�B
d�B
e,B
eFB
ezB
ezB
ezB
ezB
e�B
e�B
fB
fB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
h�B
i*B
i_B
i*B
i�B
i�B
i�B
jeB
j�B
kB
kB
kB
k6B
k6B
kQB
kQB
k�B
k�B
k�B
lWB
lWB
l�B
m)B
m)B
mCB
m�B
m�B
m�B
n/B
n}B
ncB
n�B
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
p!B
p;B
p;B
p�B
p�B
p�B
p�B
q'B
q'B
qvB
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
sB
sB
sB
s3B
s3B
shB
s�B
s�B
s�B
tnB
tnB
t�B
t�B
t�B
t�B
u?B
utB
uZB
u�B
vFB
v`B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
xB
x8B
xRB
xlB
x�B
x�B
y$B
y$B
yXB
y�B
y�B
zxB
z^B
z�B
z�B
{B
{JB
{�B
{�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}<B
}�B
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
~]B
~wB
~�B
~�B
~�B
~�B
HB
B
.B
HB
.B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105233  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191608  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191608  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191608                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041616  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041616  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                