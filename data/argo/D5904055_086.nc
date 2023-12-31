CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2016-07-06T18:01:21Z creation; 2016-07-06T18:01:21Z updated; 2016-09-02T17:52:21Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7$   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8    	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8(   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8H   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8h   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8l   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8t   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8x   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|  �|Argo profile    3.1 1.2 19500101000000  20160706180121  20181103100342  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               VA   AO  5004                            2C  D   NAVIS_A                         0303                            082713                          863 @׹X�w��1   @׹Y�[_@7?;dZ��d̓t�j1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      VA   A   A   @�ff@�33A   AffAA��A^ffA�  A�  A�  A�33A�  A�  A�  A�  B ffB  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�<�D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�p 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @}p�@��@�Q�A�\A=AZ�\A|(�A�{A�{A�G�A�{A�{A�{A�{A��GB
=B��B
=B
=B'
=B/
=B7
=B?
=BG
=BO
=BW
=B_
=Bgp�Bo
=Bv��B
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.j>D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�
D6w
D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�xRD޸RD��RD�5D�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��D�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�A�D�hR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�I�A�G�A�O�A�K�A�M�A�ZA�\)A�VA�S�A�\)A�ffA�hsA�hsA�$�A�33A�+A�"�AͶFA��A�l�A�VAɟ�A�A�I�A�+A�+A�5?AÑhA���A��wA�~�A��A�&�A�x�A�VA�oA��A���A�`BA���A�{A�dZA��uA��A��A���A��^A���A�(�A��^A�bNA�ȴA�/A��PA�O�A��HA�ȴA���A���A�t�A��A�A��/A�33A�?}A��A���A�C�A���A��hA�9XA���A�t�A��/A��DA��A��A��A�M�A��wA�r�A�A�=qA��
A��PA�-A�S�A�XA���A��A�dZA��#A�C�A�jA�oA�-A�M�A��^A��wA���A���A�~�A�|�A�VA��A�  A�;dA�  A�v�A�{A��#A��hA�ZA���A���A�1A~  A}K�A}�A|ZAx{At�AqoAn�9Am�-Al�Ak+Ah��AgoAf1Ae"�Ab��Aa;dA`A_�;A_\)A]�A\ZAY�AW�;AV�AV �AU�ATv�AT-AS��AR�HAR^5AQ�TAP�/AO�wAL$�AI��AI%AH��AHM�AG�AG�PAFn�ADE�ABv�A@�HA?�A?"�A=��A=
=A<ZA;K�A:ffA9dZA8JA7�hA7�A6�A6-A6A5�A5\)A4ȴA1�A/�^A-��A-��A,�HA+�7A)�;A(�RA({A'�A&�A%?}A$1A"$�A!7LA ��A ~�A�A�!A\)A�^A�A�yA��AhsA�jA��A�9A+AI�A  A��A�9A��A�hAx�A�`AbNA$�A�A
(�A	�;A	�7A�A(�AƨA��A��A�A\)Av�A9XA�A��AO�@�@���@�  @��w@��@�-@�?}@�j@��@�5?@�7L@�D@�b@���@�@�@� �@��-@�r�@���@�ƨ@�@��@�O�@�j@�1'@�P@�dZ@�33@�5?@�-@㝲@�`B@�l�@�~�@�V@�5?@���@ܴ9@�V@ץ�@���@҇+@Ѻ^@��m@�V@�&�@�dZ@�?}@ȴ9@�S�@�J@ź^@�O�@���@��@���@�j@��;@�~�@��-@�j@�9X@� �@��@��
@��@��H@�ff@�$�@�hs@�Z@��m@��w@�\)@�"�@��y@�v�@���@�/@��@�t�@��@���@�^5@�5?@��T@��@��9@�j@�1@��@��!@�v�@�E�@��@���@���@�hs@�V@���@�Z@�b@�\)@��@���@���@�r�@���@�l�@�33@��\@�v�@�v�@��#@���@��h@���@� �@���@�|�@�dZ@�"�@���@���@�E�@�G�@��@�b@��P@�S�@�33@�@��y@���@���@�ff@���@��F@��H@��+@�^5@�=q@�-@�{@�@���@�p�@�O�@�&�@�Ĝ@���@�G�@�G�@�G�@�`B@��@�O�@��9@�ƨ@�dZ@�|�@�;d@��y@���@�~�@�$�@���@�Q�@�J@�`B@�?}@��@�  @� �@��m@��;@��F@���@��@���@��D@��@�  @�|�@��R@���@�~�@�n�@�V@�=q@���@�`B@�%@�b@�ƨ@��F@���@�
=@�ff@�5?@��@���@��^@��@��@���@�r�@�Q�@��@���@��@���@�S�@��y@�V@��#@��7@�X@��@��@��j@� �@��w@�t�@�S�@��@��R@��!@�~�@�-@�@��T@���@��-@���@��h@�hs@�7L@�V@�Ĝ@�r�@�Z@�Q�@�Q�@�Q�@�(�@�b@��@�A�@�r�@��D@��D@��u@���@��`@�V@�X@���@��-@��@��@��@��T@��#@�?}@�7L@�&�@��@��`@��`@���@��j@��@�z�@�r�@�r�@�j@�9X@�b@�@�@��@|�@~��@~ȴ@~ff@}?}@{S�@z^5@y��@yhs@yG�@y&�@y%@x��@xĜ@xĜ@xĜ@x��@x�u@x�@xA�@w�@w�@w;d@vE�@u�@tz�@s"�@r��@q�#@q%@pĜ@pA�@o�;@o��@o��@o�w@o��@oK�@oK�@o\)@o+@n$�@m�h@m?}@mV@mV@mV@m�@m�@mV@k��@j�H@j~�@i�@i�7@hbN@h �@h�@h��@hr�@hbN@h1'@h �@g�;@gl�@f��@f��@f��@f��@fV@f{@e�T@e�h@eO�@e�@d�@dz�@d�@c��@c33@aX@`bN@_�@_;d@]@\�@\�j@\�D@\I�@\9X@[��@[�F@[C�@Z��@Z��@Z��@ZM�@Y�7@Y&�@X��@X�u@XA�@X �@W�@W��@W
=@V��@Vv�@VV@V$�@V{@U�@U�@V@V@U��@U��@U�-@U`B@T�@Tj@T(�@S�F@SdZ@SS�@So@R��@R-@QX@P��@P�9@P�u@P�u@PbN@P1'@O��@N��@N�@Nff@N$�@N{@N{@N{@N{@N{@M�T@M@M��@M�@MO�@L�/@L��@L(�@K��@K�
@K��@K��@K��@KS�@J�H@JJ@HĜ@HQ�@Hb@G�;@Gl�@F�R@E�@E/@D��@D(�@Ct�@CC�@CC�@CC�@C"�@B�!@B^5@Ahs@@A�@?�@?�;@?��@?��@?+@>��@>$�@=�-@=p�@=�@<��@<�D@<9X@;33@;@;@:��@:�!@:�\@:~�@:^5@:=q@:-@:�@9�@9X@8�`@8�9@8�@81'@7��@7K�@6��@6�+@65?@6{@5��@5`B@5V@4�@4�j@4�j@4�j@4�@4��@4z�@4Z@49X@4(�@41@3��@41@3�F@2��@2n�@2=q@2=q@2-@1�#@1hs@1G�@1�@1�@1�@1�@1�@1�@1�@0�`@0Ĝ@0�u@0r�@01'@0  @/�P@/;d@.�@.�+@.5?@-�-@,��@,�@+�m@+�F@+33@*�H@*^5@)�@)��@)��@)x�@)G�@)&�@(��@(Ĝ@(bN@(A�@(  @'�;@'�w@'|�@'\)@'+@&��@&��@&��@&�y@&�@&ȴ@&��@&�+@&E�@&@%��@%��@%�h@%�@%`B@%O�@%�@$��@$�/@$�@$j@$I�@$�@#�
@#C�@"~�@"J@!��@!�#@!��@!X@!7L@!7L@!�@ ��@ Ĝ@ �9@ r�@ Q�@  �@��@��@l�@+@
=@�y@ȴ@5?@/@��@�@I�@(�@(�@1@�m@�m@�
@ƨ@��@S�@33@�H@��@�\@�\@~�@n�@^5@�@��@�7@�7@x�@7L@�9@�@bN@Q�@Q�@Q�@A�@  @��@|�@;d@;d@
=@��@E�@{@{@{@{@{@$�@$�@{@{@@`B@/@�@��@�j@�D@I�@��@�@S�@@�H@��@��@�!@��@n�@M�@J@��@�@��@��@x�@G�@�@��@��@Ĝ@�9@��@r�@1'@�;@��@l�@K�@+@�@�@ff@5?@�@��@��@��@�h@`B@O�@O�@?}@/@�@V@V@�@�/@�@j@I�@�@��@�m@�@t�@dZ@"�@
�!@
�\@
~�@
^5@
-@	�#@	��@	X@�9@Q�@A�@�@�w@�@�P@�P@l�@
=@ȴ@�+@ff@V@$�@@��@�@�@�@`B@?}@V@�/@��@�j@��@z�@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�I�A�G�A�O�A�K�A�M�A�ZA�\)A�VA�S�A�\)A�ffA�hsA�hsA�$�A�33A�+A�"�AͶFA��A�l�A�VAɟ�A�A�I�A�+A�+A�5?AÑhA���A��wA�~�A��A�&�A�x�A�VA�oA��A���A�`BA���A�{A�dZA��uA��A��A���A��^A���A�(�A��^A�bNA�ȴA�/A��PA�O�A��HA�ȴA���A���A�t�A��A�A��/A�33A�?}A��A���A�C�A���A��hA�9XA���A�t�A��/A��DA��A��A��A�M�A��wA�r�A�A�=qA��
A��PA�-A�S�A�XA���A��A�dZA��#A�C�A�jA�oA�-A�M�A��^A��wA���A���A�~�A�|�A�VA��A�  A�;dA�  A�v�A�{A��#A��hA�ZA���A���A�1A~  A}K�A}�A|ZAx{At�AqoAn�9Am�-Al�Ak+Ah��AgoAf1Ae"�Ab��Aa;dA`A_�;A_\)A]�A\ZAY�AW�;AV�AV �AU�ATv�AT-AS��AR�HAR^5AQ�TAP�/AO�wAL$�AI��AI%AH��AHM�AG�AG�PAFn�ADE�ABv�A@�HA?�A?"�A=��A=
=A<ZA;K�A:ffA9dZA8JA7�hA7�A6�A6-A6A5�A5\)A4ȴA1�A/�^A-��A-��A,�HA+�7A)�;A(�RA({A'�A&�A%?}A$1A"$�A!7LA ��A ~�A�A�!A\)A�^A�A�yA��AhsA�jA��A�9A+AI�A  A��A�9A��A�hAx�A�`AbNA$�A�A
(�A	�;A	�7A�A(�AƨA��A��A�A\)Av�A9XA�A��AO�@�@���@�  @��w@��@�-@�?}@�j@��@�5?@�7L@�D@�b@���@�@�@� �@��-@�r�@���@�ƨ@�@��@�O�@�j@�1'@�P@�dZ@�33@�5?@�-@㝲@�`B@�l�@�~�@�V@�5?@���@ܴ9@�V@ץ�@���@҇+@Ѻ^@��m@�V@�&�@�dZ@�?}@ȴ9@�S�@�J@ź^@�O�@���@��@���@�j@��;@�~�@��-@�j@�9X@� �@��@��
@��@��H@�ff@�$�@�hs@�Z@��m@��w@�\)@�"�@��y@�v�@���@�/@��@�t�@��@���@�^5@�5?@��T@��@��9@�j@�1@��@��!@�v�@�E�@��@���@���@�hs@�V@���@�Z@�b@�\)@��@���@���@�r�@���@�l�@�33@��\@�v�@�v�@��#@���@��h@���@� �@���@�|�@�dZ@�"�@���@���@�E�@�G�@��@�b@��P@�S�@�33@�@��y@���@���@�ff@���@��F@��H@��+@�^5@�=q@�-@�{@�@���@�p�@�O�@�&�@�Ĝ@���@�G�@�G�@�G�@�`B@��@�O�@��9@�ƨ@�dZ@�|�@�;d@��y@���@�~�@�$�@���@�Q�@�J@�`B@�?}@��@�  @� �@��m@��;@��F@���@��@���@��D@��@�  @�|�@��R@���@�~�@�n�@�V@�=q@���@�`B@�%@�b@�ƨ@��F@���@�
=@�ff@�5?@��@���@��^@��@��@���@�r�@�Q�@��@���@��@���@�S�@��y@�V@��#@��7@�X@��@��@��j@� �@��w@�t�@�S�@��@��R@��!@�~�@�-@�@��T@���@��-@���@��h@�hs@�7L@�V@�Ĝ@�r�@�Z@�Q�@�Q�@�Q�@�(�@�b@��@�A�@�r�@��D@��D@��u@���@��`@�V@�X@���@��-@��@��@��@��T@��#@�?}@�7L@�&�@��@��`@��`@���@��j@��@�z�@�r�@�r�@�j@�9X@�b@�@�@��@|�@~��@~ȴ@~ff@}?}@{S�@z^5@y��@yhs@yG�@y&�@y%@x��@xĜ@xĜ@xĜ@x��@x�u@x�@xA�@w�@w�@w;d@vE�@u�@tz�@s"�@r��@q�#@q%@pĜ@pA�@o�;@o��@o��@o�w@o��@oK�@oK�@o\)@o+@n$�@m�h@m?}@mV@mV@mV@m�@m�@mV@k��@j�H@j~�@i�@i�7@hbN@h �@h�@h��@hr�@hbN@h1'@h �@g�;@gl�@f��@f��@f��@f��@fV@f{@e�T@e�h@eO�@e�@d�@dz�@d�@c��@c33@aX@`bN@_�@_;d@]@\�@\�j@\�D@\I�@\9X@[��@[�F@[C�@Z��@Z��@Z��@ZM�@Y�7@Y&�@X��@X�u@XA�@X �@W�@W��@W
=@V��@Vv�@VV@V$�@V{@U�@U�@V@V@U��@U��@U�-@U`B@T�@Tj@T(�@S�F@SdZ@SS�@So@R��@R-@QX@P��@P�9@P�u@P�u@PbN@P1'@O��@N��@N�@Nff@N$�@N{@N{@N{@N{@N{@M�T@M@M��@M�@MO�@L�/@L��@L(�@K��@K�
@K��@K��@K��@KS�@J�H@JJ@HĜ@HQ�@Hb@G�;@Gl�@F�R@E�@E/@D��@D(�@Ct�@CC�@CC�@CC�@C"�@B�!@B^5@Ahs@@A�@?�@?�;@?��@?��@?+@>��@>$�@=�-@=p�@=�@<��@<�D@<9X@;33@;@;@:��@:�!@:�\@:~�@:^5@:=q@:-@:�@9�@9X@8�`@8�9@8�@81'@7��@7K�@6��@6�+@65?@6{@5��@5`B@5V@4�@4�j@4�j@4�j@4�@4��@4z�@4Z@49X@4(�@41@3��@41@3�F@2��@2n�@2=q@2=q@2-@1�#@1hs@1G�@1�@1�@1�@1�@1�@1�@1�@0�`@0Ĝ@0�u@0r�@01'@0  @/�P@/;d@.�@.�+@.5?@-�-@,��@,�@+�m@+�F@+33@*�H@*^5@)�@)��@)��@)x�@)G�@)&�@(��@(Ĝ@(bN@(A�@(  @'�;@'�w@'|�@'\)@'+@&��@&��@&��@&�y@&�@&ȴ@&��@&�+@&E�@&@%��@%��@%�h@%�@%`B@%O�@%�@$��@$�/@$�@$j@$I�@$�@#�
@#C�@"~�@"J@!��@!�#@!��@!X@!7L@!7L@!�@ ��@ Ĝ@ �9@ r�@ Q�@  �@��@��@l�@+@
=@�y@ȴ@5?@/@��@�@I�@(�@(�@1@�m@�m@�
@ƨ@��@S�@33@�H@��@�\@�\@~�@n�@^5@�@��@�7@�7@x�@7L@�9@�@bN@Q�@Q�@Q�@A�@  @��@|�@;d@;d@
=@��@E�@{@{@{@{@{@$�@$�@{@{@@`B@/@�@��@�j@�D@I�@��@�@S�@@�H@��@��@�!@��@n�@M�@J@��@�@��@��@x�@G�@�@��@��@Ĝ@�9@��@r�@1'@�;@��@l�@K�@+@�@�@ff@5?@�@��@��@��@�h@`B@O�@O�@?}@/@�@V@V@�@�/@�@j@I�@�@��@�m@�@t�@dZ@"�@
�!@
�\@
~�@
^5@
-@	�#@	��@	X@�9@Q�@A�@�@�w@�@�P@�P@l�@
=@ȴ@�+@ff@V@$�@@��@�@�@�@`B@?}@V@�/@��@�j@��@z�@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BhBB�Bs�B�oB�jBĜB��B�B�/B�TB�yB��B��B��B1B�B�B"�B&�B.B/B2-B33B2-B7LBC�BE�B@�B/B)�B%�B$�B#�B!�B�B�BhB	7BB��B��B��B��B��B�B�BB��B��BĜB�wB�FB�'B�B��B��B�{B�bB�DB�%B}�Bm�B[#BJ�B>wB49B,B(�B.B%�B �B�BuB  B��B��B�BɺB�=B�Bn�By�B�1B�VB�1Bw�BbNB<jB-B1'B/B�BoBB
�B
�B
�NB
�`B
�`B
�HB
��B
�dB
�B
��B
��B
��B
�\B
p�B
P�B
.B
�B
VB
1B	��B	�B	�B	��B	ŢB	�RB	�!B	��B	��B	��B	�uB	�VB	�B	y�B	t�B	n�B	hsB	dZB	bNB	`BB	[#B	XB	T�B	M�B	E�B	6FB	,B	(�B	'�B	%�B	#�B	!�B	�B	oB	JB	B��B��B��B�B�B�B�B�sB�`B�ZB�TB�HB�BB�;B�5B�)B�B��B��B�RB�FB�-B�B��B��B��B��B��B��B��B��B��B��B�{B�bB�PB�1B�Bz�Bx�Bv�Bq�Bl�BjBgmBffBe`BdZBbNBaHBbNBbNBcTBcTBcTBbNBbNBbNBbNB`BB^5B_;B_;B^5B^5B]/B]/B]/B]/B]/B\)B[#B\)B^5B^5B^5B^5B^5B_;B_;B_;B`BB`BB`BB`BB`BB`BB^5B]/B]/B_;B`BB`BB`BBaHBaHBaHBaHBbNBaHBaHBbNBbNBbNBbNBcTBcTBbNBaHB`BB]/B\)BXBVBVBVBXBYBZB^5BbNBbNBe`BgmBhsBhsBiyBiyBiyBiyBjBm�Bo�Bs�Bs�Bs�Bs�Bs�Bt�Bu�Bv�Bv�Bx�B|�B~�B~�B�B�B�B�B�B�1B�=B�JB�PB�VB�\B�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�LB�}BÖBɺB��B��B��B��B��B�B�B�B�)B�HB�`B�`B�`B�mB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B		7B	
=B	PB	VB	VB	bB	uB	�B	�B	�B	�B	!�B	&�B	'�B	.B	0!B	33B	5?B	7LB	:^B	>wB	C�B	E�B	G�B	H�B	H�B	J�B	I�B	F�B	F�B	H�B	E�B	E�B	H�B	J�B	K�B	L�B	M�B	M�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	O�B	P�B	Q�B	R�B	R�B	S�B	S�B	S�B	T�B	VB	W
B	XB	YB	ZB	[#B	\)B	\)B	]/B	_;B	cTB	dZB	e`B	gmB	iyB	jB	k�B	l�B	p�B	u�B	x�B	{�B	|�B	~�B	� B	�B	�%B	�7B	�DB	�JB	�VB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�LB	�RB	�^B	�qB	��B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�/B	�BB	�BB	�HB	�TB	�TB	�TB	�NB	�HB	�HB	�TB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B

=B
JB
JB
JB
PB
PB
PB
PB
VB
\B
\B
\B
\B
\B
bB
bB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
/B
/B
/B
0!B
2-B
33B
33B
33B
33B
33B
49B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
VB
VB
VB
VB
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
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
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
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
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
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
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
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�;B�BI�By�B��B�}B��B�B�)B�HB�sB�B��B��BBbB!�B"�B&�B0!B33B2-B49B5?B5?B;dBG�BM�BI�B33B-B'�B&�B%�B%�B�B�B�BVBJBB��B��B��B��B��B�sB�)B��BɺBĜB�dB�?B�FB��B��B��B�uB�VB�=B�%Bu�BbNBO�BC�B8RB/B-B33B)�B#�B�B�B%B��BB�B�B�VB�=Bq�Bz�B�7B�oB�PB}�Bl�BC�B/B33B49B!�B�BDB
��B
�B
�`B
�sB
�sB
�fB
�;B
��B
�3B
��B
��B
��B
��B
y�B
ZB
5?B
�B
hB
VB
%B	�B	�)B	��B	��B	�qB	�?B	��B	��B	��B	��B	��B	�7B	}�B	x�B	r�B	k�B	ffB	e`B	dZB	^5B	[#B	YB	R�B	P�B	>wB	/B	+B	+B	(�B	&�B	&�B	!�B	�B	oB		7B	B	  B��B��B��B��B�B�B�sB�mB�fB�ZB�NB�NB�HB�BB�ZB��BǮB�dB�^B�RB�9B�B��B��B��B��B�B��B��B��B��B��B��B�uB�\B�JB|�B{�B|�Bz�Bq�Bo�Bm�BjBhsBgmBffBe`BdZBdZBffBffBe`BffBffBdZBdZBcTBffBbNBaHBbNBaHB`BB`BB_;B_;B_;B_;B`BB`BB`BB`BB`BBaHBaHBbNBaHBcTBcTBbNBbNBbNBbNBcTBcTBbNB`BBaHBbNBbNBdZBdZBcTBcTBdZBdZBcTBdZBdZBgmBgmBffBffBe`BdZBcTBcTBdZB`BB]/B[#BYBZB\)B\)B^5BbNBdZBffBhsBiyBjBjBk�Bk�Bk�Bl�Bn�Bp�Br�Bu�Bu�Bu�Bu�Bu�Bw�Bw�Bx�By�B{�B~�B�B�B�B�B�B�B�+B�DB�PB�VB�\B�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�RB�^B��BƨB��B��B��B��B��B��B�B�B�#B�;B�ZB�mB�mB�mB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	1B	DB	JB	\B	bB	bB	oB	�B	�B	�B	�B	�B	#�B	(�B	)�B	0!B	2-B	5?B	8RB	9XB	<jB	@�B	E�B	G�B	I�B	J�B	J�B	M�B	M�B	I�B	H�B	J�B	H�B	G�B	J�B	L�B	M�B	N�B	Q�B	P�B	P�B	Q�B	Q�B	Q�B	Q�B	Q�B	Q�B	Q�B	R�B	S�B	T�B	T�B	VB	W
B	VB	W
B	XB	YB	ZB	[#B	\)B	]/B	^5B	^5B	_;B	aHB	e`B	ffB	gmB	iyB	k�B	l�B	m�B	n�B	r�B	w�B	z�B	}�B	~�B	�B	�B	�B	�1B	�DB	�PB	�VB	�bB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�9B	�XB	�XB	�dB	�wB	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�NB	�NB	�TB	�`B	�`B	�`B	�`B	�TB	�TB	�`B	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
JB
VB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
2-B
49B
5?B
5?B
5?B
5?B
5?B
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
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
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
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
bNB
bNB
bNB
bNB
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
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
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
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.24 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 1 (+/-0), vertically averaged dS = 0.002 (+/-0.001)                                                                                                                              Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201811021508402018110215084020181102150840  AO  ARCAADJP                                                                    20160706180121    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160706180121  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160706180121  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160929165029  QC  PRES            @�ffD�p G�O�                PM  ARSQCTM V1.1                                                                20160929165029  QC  PSAL            @�ffD�p G�O�                PM  ARSQOWGUV1.0                                                                20181103100342  IP                  G�O�G�O�G�O�                