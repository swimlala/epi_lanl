CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-02-28T10:00:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210228100051  20210228100051  4902949 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               }A   AO  6976                            2B  A   NAVIS_A                         0823                            170210                          863 @�a�pB�1   @�a��f`@0���v��cHA�7K�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         }A   A   A   @���@�  A   A   A@  A^ffA�  A���A�  A�  A�  A�  A�  A���B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@�Q�A(�A<(�AZ�\A|(�A��HA�{A�{A�{A�{A�{A��HA�{B
=Bp�B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCS�)CUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�;�D�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��D�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�xRD޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD���D�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�;�D�hR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�JA�VA��A��A��A��A� �A��A��A�"�A�"�A�$�A�"�A� �A�"�A�"�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�+A�+A�-A�-A�/A�1'A�1'A�1'A�/A�1'A�1'A�33A�33A�33A�33A�33A�5?A�+A�(�A�-A�+A�(�A�/A�+A�oA� �A��A��yA�ZA�`BA�O�A��A���A�-A�&�A�~�A��A��HA��A���A�v�A�Q�A���A�jA�-A�dZA��-A���A���A�v�A�oA�ȴA�VA��PA��A��yA���A�;dA�I�A��A�S�A���A��FA��A���A�ƨA��;A�VA���A���A���A��yA�5?A��#A���A�(�A���A��9A�(�A���A��A�%A�$�A��A��uA�
=A���A�S�A�;dA���Ap�Au��AsC�ApI�AnVAln�AihsAfȴAb��A_�wA]�A[��AWdZAU�^AS?}AO�#AN�RANM�AM��AKG�AF9XAB�RA@I�A> �A=��A=p�A<1'A9�;A6�A5O�A4M�A2~�A0ȴA-A*�9A(r�A%�wA$��A#��A!ƨA {A��AG�A~�A�A�A�TA{A �AK�A�A��A�jAI�A�FA�AO�An�A5?A�;A��AQ�A�;A  A\)A9XA+A$�A\)A	�A	�7A��A5?A	��A	A	l�A��A�A-A��A�AQ�A��A\)A�A�jA�!A�A�PAp�A�`A��A��AAjAA�-AXA �9@��y@��H@���@�J@�V@�b@��P@��y@��@�Ĝ@�n�@���@�5?@�1@�1@��j@�%@��@�7L@��/@���@���@�/@�b@�^@�{@�@��@���@��H@��@�I�@��H@�-@��@���@�+@���@旍@�-@��@�z�@�A�@�1'@�(�@��y@��@�b@��@��/@��`@�X@ݙ�@�@��#@��@�{@��#@�O�@��;@�=q@�ff@�ȴ@��y@�@�l�@ְ!@׍P@�r�@�l�@�-@�hs@�K�@У�@�\)@�^5@�@͑h@�p�@���@� �@̴9@���@� �@�
=@ˮ@��
@�1'@�z�@̛�@˶F@˶F@ˮ@��@�v�@�$�@ȣ�@Ǿw@�;d@���@�G�@Ĭ@�r�@�5?@�1@��H@�@���@���@��^@�@���@�x�@�{@�p�@�O�@��j@��m@�K�@��\@���@���@�p�@�&�@��@�bN@�(�@��w@�K�@���@��h@��D@�  @��m@��F@�o@���@��@���@�O�@�A�@�o@�n�@���@�?}@�r�@��P@�C�@��@���@�E�@�p�@���@���@�x�@�O�@���@�J@���@��@�$�@��h@�Q�@�A�@�A�@� �@�1@���@�|�@�C�@�
=@��H@��@�ff@�$�@�V@�ff@��@�S�@��R@�~�@�n�@�=q@�@��7@�O�@��9@�"�@���@��+@�E�@���@�hs@���@��`@�Ĝ@��@�(�@��
@��@�l�@��@��H@��R@��@��@�7L@��/@�r�@� �@���@��w@�C�@���@��+@�E�@�-@�J@���@��#@��#@���@�?}@��`@���@�bN@��;@���@��@�t�@�S�@�;d@�
=@��@�5?@���@�p�@���@��@��@�ƨ@��F@��@��P@�l�@�K�@�o@��@��!@�$�@�@�hs@��@�%@���@��@��9@��F@�;d@�"�@�n�@�@��#@���@�V@�j@�A�@�1@��m@�ƨ@��F@���@�t�@��y@��R@�ff@�-@�J@��@�@�hs@��^@�V@�{@���@���@���@�G�@�/@��@���@�bN@�A�@� �@��@��F@��P@�S�@�@��y@�ȴ@��!@���@��\@�p�@��@��u@�j@���@���@�\)@�\)@�S�@��@���@��+@�v�@�E�@��@���@��@��@��@��\@�~�@�M�@���@���@�p�@�?}@��`@�j@� �@�@
=@~�+@~$�@}�T@}�-@}`B@|��@{�m@{��@{��@{��@{dZ@{33@z�!@zJ@y�7@yG�@x��@xbN@xA�@w�@w�P@wl�@w\)@v�@vE�@v@u�@u�@u�@u��@t��@tZ@sC�@q�@q7L@q%@pr�@pb@pb@p �@pb@o�;@o|�@o+@n5?@m��@m`B@m/@l�D@l1@kt�@ko@j�H@j��@j-@i��@i�@h��@hr�@hA�@h1'@hb@g��@gl�@g;d@g
=@f�y@fȴ@f��@fv�@f{@ep�@e/@d��@dz�@d9X@cƨ@ct�@c"�@b��@b�\@bn�@b�@a��@a��@a&�@`�`@`��@`�@` �@_l�@^�R@^5?@]�-@]�@\�@[�m@[�@[C�@[o@Z=q@YG�@XĜ@X�@XQ�@W��@W+@V�y@U�T@U?}@T��@TZ@T9X@T(�@T�@T1@Sƨ@SS�@Rn�@Q��@Q%@Pb@O��@OK�@N��@N�@N5?@N@M��@M?}@L��@L��@Lz�@L1@K��@K33@J��@J=q@I�@Ix�@I%@HQ�@G�@G��@G�@G|�@F��@F��@F��@Fv�@FE�@F@E`B@D��@D��@D�@D��@D�D@Dz�@DI�@D9X@D�@C�F@C33@B�H@B�!@B=q@Ahs@@�`@@��@@1'@@b@?|�@>ȴ@>��@>v�@>E�@>{@=�T@=p�@<��@<�@<�D@<9X@;�m@;��@;��@;��@;��@;��@;��@;dZ@:�H@:�\@:^5@9�#@9�^@9�7@9G�@8�`@8Ĝ@8Ĝ@8Ĝ@8��@8r�@8A�@81'@8b@8  @7�@7�;@7��@7�@7|�@7;d@6��@6�y@6ȴ@6�+@6E�@5�@5p�@4�D@4Z@41@3��@3��@3��@3t�@2��@2��@2~�@2^5@2J@1��@1��@1�^@1��@1��@1��@1&�@0Ĝ@0�u@0�@0r�@0A�@/��@/�@/��@/|�@/K�@.�y@.��@.v�@.V@.@-�h@-`B@-/@-�@-V@,�@,�/@,�D@+�
@+�@*�@*^5@*M�@*�@)��@)�7@)&�@)�@(Ĝ@(r�@(1'@(b@'��@'��@'K�@&$�@%/@$Z@$I�@$(�@#ƨ@#33@"�\@"-@!��@!��@!�7@!hs@!hs@!7L@!&�@!&�@!&�@!�@ bN@  �@ b@��@�@�@|�@K�@;d@�@
=@�y@�R@��@ff@E�@5?@5?@$�@@�@�T@�-@�h@`B@V@j@�m@��@S�@�@�!@��@~�@=q@�@J@��@��@x�@X@�@�9@r�@A�@ �@ �@b@�w@�P@|�@|�@\)@\)@K�@K�@K�@;d@�@��@ȴ@ff@��@?}@��@Z@(�@�@��@�
@�@C�@�@n�@-@��@�#@��@�^@�^@��@hs@7L@�`@�u@A�@1'@  @��@��@��@��@��@��@��@��@��@�@\)@
=@�R@��@�+@v�@ff@ff@V@5?@{@�-@�h@�@p�@O�@?}@�@V@��@��@j@�@�m@�
@�F@�@C�@o@
�@
��@
��@
��@
^5@
-@	��@	�#@	��@	�^@	��@	x�@	hs@	G�@	7L@	&�@�`@�`@��@��@�@�@�@r�@r�@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�  A�JA�VA��A��A��A��A� �A��A��A�"�A�"�A�$�A�"�A� �A�"�A�"�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�+A�+A�-A�-A�/A�1'A�1'A�1'A�/A�1'A�1'A�33A�33A�33A�33A�33A�5?A�+A�(�A�-A�+A�(�A�/A�+A�oA� �A��A��yA�ZA�`BA�O�A��A���A�-A�&�A�~�A��A��HA��A���A�v�A�Q�A���A�jA�-A�dZA��-A���A���A�v�A�oA�ȴA�VA��PA��A��yA���A�;dA�I�A��A�S�A���A��FA��A���A�ƨA��;A�VA���A���A���A��yA�5?A��#A���A�(�A���A��9A�(�A���A��A�%A�$�A��A��uA�
=A���A�S�A�;dA���Ap�Au��AsC�ApI�AnVAln�AihsAfȴAb��A_�wA]�A[��AWdZAU�^AS?}AO�#AN�RANM�AM��AKG�AF9XAB�RA@I�A> �A=��A=p�A<1'A9�;A6�A5O�A4M�A2~�A0ȴA-A*�9A(r�A%�wA$��A#��A!ƨA {A��AG�A~�A�A�A�TA{A �AK�A�A��A�jAI�A�FA�AO�An�A5?A�;A��AQ�A�;A  A\)A9XA+A$�A\)A	�A	�7A��A5?A	��A	A	l�A��A�A-A��A�AQ�A��A\)A�A�jA�!A�A�PAp�A�`A��A��AAjAA�-AXA �9@��y@��H@���@�J@�V@�b@��P@��y@��@�Ĝ@�n�@���@�5?@�1@�1@��j@�%@��@�7L@��/@���@���@�/@�b@�^@�{@�@��@���@��H@��@�I�@��H@�-@��@���@�+@���@旍@�-@��@�z�@�A�@�1'@�(�@��y@��@�b@��@��/@��`@�X@ݙ�@�@��#@��@�{@��#@�O�@��;@�=q@�ff@�ȴ@��y@�@�l�@ְ!@׍P@�r�@�l�@�-@�hs@�K�@У�@�\)@�^5@�@͑h@�p�@���@� �@̴9@���@� �@�
=@ˮ@��
@�1'@�z�@̛�@˶F@˶F@ˮ@��@�v�@�$�@ȣ�@Ǿw@�;d@���@�G�@Ĭ@�r�@�5?@�1@��H@�@���@���@��^@�@���@�x�@�{@�p�@�O�@��j@��m@�K�@��\@���@���@�p�@�&�@��@�bN@�(�@��w@�K�@���@��h@��D@�  @��m@��F@�o@���@��@���@�O�@�A�@�o@�n�@���@�?}@�r�@��P@�C�@��@���@�E�@�p�@���@���@�x�@�O�@���@�J@���@��@�$�@��h@�Q�@�A�@�A�@� �@�1@���@�|�@�C�@�
=@��H@��@�ff@�$�@�V@�ff@��@�S�@��R@�~�@�n�@�=q@�@��7@�O�@��9@�"�@���@��+@�E�@���@�hs@���@��`@�Ĝ@��@�(�@��
@��@�l�@��@��H@��R@��@��@�7L@��/@�r�@� �@���@��w@�C�@���@��+@�E�@�-@�J@���@��#@��#@���@�?}@��`@���@�bN@��;@���@��@�t�@�S�@�;d@�
=@��@�5?@���@�p�@���@��@��@�ƨ@��F@��@��P@�l�@�K�@�o@��@��!@�$�@�@�hs@��@�%@���@��@��9@��F@�;d@�"�@�n�@�@��#@���@�V@�j@�A�@�1@��m@�ƨ@��F@���@�t�@��y@��R@�ff@�-@�J@��@�@�hs@��^@�V@�{@���@���@���@�G�@�/@��@���@�bN@�A�@� �@��@��F@��P@�S�@�@��y@�ȴ@��!@���@��\@�p�@��@��u@�j@���@���@�\)@�\)@�S�@��@���@��+@�v�@�E�@��@���@��@��@��@��\@�~�@�M�@���@���@�p�@�?}@��`@�j@� �@�@
=@~�+@~$�@}�T@}�-@}`B@|��@{�m@{��@{��@{��@{dZ@{33@z�!@zJ@y�7@yG�@x��@xbN@xA�@w�@w�P@wl�@w\)@v�@vE�@v@u�@u�@u�@u��@t��@tZ@sC�@q�@q7L@q%@pr�@pb@pb@p �@pb@o�;@o|�@o+@n5?@m��@m`B@m/@l�D@l1@kt�@ko@j�H@j��@j-@i��@i�@h��@hr�@hA�@h1'@hb@g��@gl�@g;d@g
=@f�y@fȴ@f��@fv�@f{@ep�@e/@d��@dz�@d9X@cƨ@ct�@c"�@b��@b�\@bn�@b�@a��@a��@a&�@`�`@`��@`�@` �@_l�@^�R@^5?@]�-@]�@\�@[�m@[�@[C�@[o@Z=q@YG�@XĜ@X�@XQ�@W��@W+@V�y@U�T@U?}@T��@TZ@T9X@T(�@T�@T1@Sƨ@SS�@Rn�@Q��@Q%@Pb@O��@OK�@N��@N�@N5?@N@M��@M?}@L��@L��@Lz�@L1@K��@K33@J��@J=q@I�@Ix�@I%@HQ�@G�@G��@G�@G|�@F��@F��@F��@Fv�@FE�@F@E`B@D��@D��@D�@D��@D�D@Dz�@DI�@D9X@D�@C�F@C33@B�H@B�!@B=q@Ahs@@�`@@��@@1'@@b@?|�@>ȴ@>��@>v�@>E�@>{@=�T@=p�@<��@<�@<�D@<9X@;�m@;��@;��@;��@;��@;��@;��@;dZ@:�H@:�\@:^5@9�#@9�^@9�7@9G�@8�`@8Ĝ@8Ĝ@8Ĝ@8��@8r�@8A�@81'@8b@8  @7�@7�;@7��@7�@7|�@7;d@6��@6�y@6ȴ@6�+@6E�@5�@5p�@4�D@4Z@41@3��@3��@3��@3t�@2��@2��@2~�@2^5@2J@1��@1��@1�^@1��@1��@1��@1&�@0Ĝ@0�u@0�@0r�@0A�@/��@/�@/��@/|�@/K�@.�y@.��@.v�@.V@.@-�h@-`B@-/@-�@-V@,�@,�/@,�D@+�
@+�@*�@*^5@*M�@*�@)��@)�7@)&�@)�@(Ĝ@(r�@(1'@(b@'��@'��@'K�@&$�@%/@$Z@$I�@$(�@#ƨ@#33@"�\@"-@!��@!��@!�7@!hs@!hs@!7L@!&�@!&�@!&�@!�@ bN@  �@ b@��@�@�@|�@K�@;d@�@
=@�y@�R@��@ff@E�@5?@5?@$�@@�@�T@�-@�h@`B@V@j@�m@��@S�@�@�!@��@~�@=q@�@J@��@��@x�@X@�@�9@r�@A�@ �@ �@b@�w@�P@|�@|�@\)@\)@K�@K�@K�@;d@�@��@ȴ@ff@��@?}@��@Z@(�@�@��@�
@�@C�@�@n�@-@��@�#@��@�^@�^@��@hs@7L@�`@�u@A�@1'@  @��@��@��@��@��@��@��@��@��@�@\)@
=@�R@��@�+@v�@ff@ff@V@5?@{@�-@�h@�@p�@O�@?}@�@V@��@��@j@�@�m@�
@�F@�@C�@o@
�@
��@
��@
��@
^5@
-@	��@	�#@	��@	�^@	��@	x�@	hs@	G�@	7L@	&�@�`@�`@��@��@�@�@�@r�@r�@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
W
B
XB
XB
XB
XB
XB
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
W
B
W
B
W
B
XB
W
B
XB
cTB
�B
��B
�ZB
�B
��BuB0!BC�BYBu�B�B��B��B�XB��B��B�B�5B�/B�ZB�B��B+BPB	7B
=B�BB
=B1B��B��B  B��B�NB��B��BȴB�RB��B��B�\B�%B|�Bt�Bo�BhsB_;BYBT�BM�BF�B>wB7LB%�B	7B
��B
�B
��B
�-B
|�B
e`B
\)B
G�B
;dB
$�B	�B	��B	�B	��B	�uB	� B	l�B	^5B	J�B	A�B	=qB	33B	!�B	�B	�B	�B	�B	�B	�B��B�HB��B��B�B�#B�ZB�yB�HB�BB�`B�TB�TB�B��B�FB�3B�9B�LB�RB�RB�FB�9B�qB�qB�^B�LB�XB�?B�FB�^B�jB�qB�wB�wB�}B�}B�wB�}B��B��B��B��B�BB�TB�)B�BB�NB�;B�)B�;B�NB�B	!�B	"�B	#�B	"�B	)�B	/B	=qB	=qB	=qB	?}B	>wB	>wB	@�B	@�B	E�B	G�B	K�B	Q�B	VB	\)B	cTB	ffB	dZB	e`B	cTB	`BB	\)B	]/B	^5B	`BB	dZB	gmB	e`B	e`B	bNB	_;B	[#B	XB	^5B	u�B	u�B	{�B	� B	�%B	�JB	�hB	�{B	�\B	�JB	�=B	�+B	{�B	�B	�B	�B	�\B	�bB	�DB	�%B	�B	� B	�B	|�B	� B	�B	�B	�B	� B	~�B	~�B	~�B	� B	~�B	}�B	z�B	u�B	z�B	�B	�+B	�7B	�DB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	��B	��B	��B	��B	�oB	�hB	�hB	�uB	�{B	�uB	�{B	��B	��B	��B	��B	��B	�B	�B	�3B	�XB	�FB	�XB	�dB	�dB	�^B	�dB	�dB	�FB	�?B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�qB	��B	ÖB	ÖB	ĜB	ŢB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	B	��B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ĜB	��B	��B	��B	��B	��B	�)B	�/B	�5B	�;B	�HB	�NB	�NB	�ZB	�`B	�mB	�mB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B

=B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
DB

=B

=B
	7B
	7B
	7B
+B
+B
%B
+B
+B
+B
1B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
JB
VB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
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
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
)�B
)�B
)�B
+B
+B
)�B
'�B
&�B
&�B
'�B
'�B
(�B
)�B
+B
,B
,B
,B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
A�B
A�B
B�B
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
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
XB
YB
YB
YB
ZB
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
[#B
[#B
[#B
\)B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
_;B
`BB
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
cTB
cTB
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
jB
jB
jB
jB
jB
jB
k�B
l�B
m�B
m�B
m�B
m�B
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
o�B
o�B
o�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
W
B
XB
XB
XB
XB
XB
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
W
B
W
B
W
B
XB
W
B
XB
cTB
�B
��B
�ZB
�B
��BuB0!BC�BYBu�B�B��B��B�XB��B��B�B�5B�/B�ZB�B��B+BPB	7B
=B�BB
=B1B��B��B  B��B�NB��B��BȴB�RB��B��B�\B�%B|�Bt�Bo�BhsB_;BYBT�BM�BF�B>wB7LB%�B	7B
��B
�B
��B
�-B
|�B
e`B
\)B
G�B
;dB
$�B	�B	��B	�B	��B	�uB	� B	l�B	^5B	J�B	A�B	=qB	33B	!�B	�B	�B	�B	�B	�B	�B��B�HB��B��B�B�#B�ZB�yB�HB�BB�`B�TB�TB�B��B�FB�3B�9B�LB�RB�RB�FB�9B�qB�qB�^B�LB�XB�?B�FB�^B�jB�qB�wB�wB�}B�}B�wB�}B��B��B��B��B�BB�TB�)B�BB�NB�;B�)B�;B�NB�B	!�B	"�B	#�B	"�B	)�B	/B	=qB	=qB	=qB	?}B	>wB	>wB	@�B	@�B	E�B	G�B	K�B	Q�B	VB	\)B	cTB	ffB	dZB	e`B	cTB	`BB	\)B	]/B	^5B	`BB	dZB	gmB	e`B	e`B	bNB	_;B	[#B	XB	^5B	u�B	u�B	{�B	� B	�%B	�JB	�hB	�{B	�\B	�JB	�=B	�+B	{�B	�B	�B	�B	�\B	�bB	�DB	�%B	�B	� B	�B	|�B	� B	�B	�B	�B	� B	~�B	~�B	~�B	� B	~�B	}�B	z�B	u�B	z�B	�B	�+B	�7B	�DB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	��B	��B	��B	��B	�oB	�hB	�hB	�uB	�{B	�uB	�{B	��B	��B	��B	��B	��B	�B	�B	�3B	�XB	�FB	�XB	�dB	�dB	�^B	�dB	�dB	�FB	�?B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�qB	��B	ÖB	ÖB	ĜB	ŢB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	B	��B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ĜB	��B	��B	��B	��B	��B	�)B	�/B	�5B	�;B	�HB	�NB	�NB	�ZB	�`B	�mB	�mB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B

=B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
DB

=B

=B
	7B
	7B
	7B
+B
+B
%B
+B
+B
+B
1B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
JB
VB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
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
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
)�B
)�B
)�B
+B
+B
)�B
'�B
&�B
&�B
'�B
'�B
(�B
)�B
+B
,B
,B
,B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
A�B
A�B
B�B
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
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
XB
YB
YB
YB
ZB
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
[#B
[#B
[#B
\)B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
_;B
`BB
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
cTB
cTB
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
jB
jB
jB
jB
jB
jB
k�B
l�B
m�B
m�B
m�B
m�B
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
o�B
o�B
o�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210228100051                              AO  ARCAADJP                                                                    20210228100051    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210228100051  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210228100051  QCF$                G�O�G�O�G�O�0               