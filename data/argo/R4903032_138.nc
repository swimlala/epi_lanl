CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-04-04T09:01:04Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ox   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۈ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޸   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20220404090104  20220404090104  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @��֬��1   @���I���@;�������c��t�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D��3D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�Q�@�Q�A(�A<(�A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�
Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�5D�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDջ�D��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�xRD޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD�D�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�?}A�I�A�K�A�K�A�K�A�K�A�M�A�Q�A�S�A�O�A�O�A�I�A�G�A�C�A�;dA�A�A�I�A�S�A�M�A��A��A�A���A���A���A���A���A��A��TA���A��9A�~�A�
=A��HA��!A��\A�JA��HA��A�G�A�z�A�jA� �A��#A�VA���A���A�I�A�A�A�  A���A�%A��;A�M�A�A��HA�M�A���A���A��A�-A��FA�33A�VA�VA��/A� �A���A��-A���A�VA�^5A�
=A�$�A�~�A���A�~�A�C�A�bNA��wA�A�A��9A���A�G�A��A�&�A���A�x�A�~�A�bNA��A�9XA��!A��uA�A�A��RA���A��A�&�A�M�A�;dA�x�A��7A���A~bNA{/Ay�wAxAvA�Atv�Ap��Al��Aj��Ai�FAh�Ah�Agp�Af1AeVAc�FAa��Aa%A_�;A_oA^v�A]�;AZ��AYdZAX�AV�AT��ATVAS��AQ�mAP�`AP(�AO�-AN��AN-AM��AMt�AM�AL{AK%AI�7AG��AGl�AFbNAE��AD�AC�AB�9AA��A@ȴA@9XA>�!A=�^A=VA;�
A;O�A;K�A;G�A;
=A:r�A:�A:�A9A7&�A61'A5�FA5+A4v�A3A3;dA2��A29XA1l�A1"�A0��A0jA/�-A-��A,~�A+��A+&�A*-A)O�A(��A(A�A&~�A%�mA$�A$1A#t�A"��A!�A!C�A A�HA��AA�A`BA��A-A�A�A�`AA�A�A�DA�FA&�A��A�+A�FA�9Av�A�A�A��A��A��A&�AA�A�A��A�+A1'A��A�AK�A
I�A	�A	S�AQ�A��A`BA�`A�#A
=A1A�uAhsA ĜA J@�ƨ@�M�@��7@��/@�  @�dZ@���@��!@�{@��^@���@�"�@�J@�Ĝ@���@��@�+@�Z@�X@睲@��@�@�7L@��
@�@��@�M�@��@��@���@�bN@�o@޸R@���@�bN@�X@��
@�n�@�ƨ@�o@��@щ7@�bN@Ͼw@��y@ΰ!@��T@���@�t�@�J@ɑh@�G�@���@Ǿw@��@Ų-@��m@�E�@��@�dZ@�n�@�K�@�{@��@��m@���@��@�`B@��@�j@���@�@�ff@���@�|�@�5?@��@���@�r�@�j@���@�C�@�@�n�@��@���@�X@�z�@�dZ@�^5@��@�b@��@��F@��@��@�|�@�t�@�33@���@�hs@�\)@���@�E�@��-@���@��9@��u@��u@��D@��@�bN@�Q�@�(�@��m@���@��!@�=q@��@�@���@�?}@�%@���@��@���@�r�@�A�@�33@��h@���@���@�V@�(�@���@�
=@�^5@�@��#@��h@�/@���@���@��u@�Q�@�(�@�  @��@�5?@���@��/@�I�@��
@�\)@���@�^5@��@���@�@���@�G�@���@��@��@��
@���@�t�@�+@��@��H@��H@���@��R@���@���@��#@��@�p�@�hs@�hs@�X@�/@��@��@�V@���@�z�@�bN@�bN@�bN@�I�@�9X@�1'@�9X@�(�@��@���@���@���@���@�t�@�\)@�C�@��@�o@��H@���@��\@�~�@�ff@�V@�$�@��-@��@�hs@�G�@�%@�Ĝ@��u@�z�@�Q�@�(�@�1@�;@�@|�@~�y@~V@}@|��@|z�@|�@{��@{dZ@{@z~�@z^5@z�@y7L@x �@x  @w��@w�@w|�@wl�@wK�@v��@u�@u�@u�@tZ@s��@t1@s�
@s��@sC�@so@r�!@rM�@r�@q�@q�7@qhs@q�@pĜ@pr�@o��@o�P@o;d@n�R@nff@nff@n5?@m�@mO�@m/@l�/@l��@lZ@lI�@l�@k�F@k@j�\@j-@i�@i�@i�@i�#@i��@i%@h�u@h�@h�@hbN@h �@g�@g�;@g�@g|�@gK�@f��@f��@f@eV@d��@d�D@dz�@dI�@dI�@d9X@d�@c��@c��@ct�@c33@b�H@a��@a�^@a7L@a�@`��@`�@`bN@`A�@`1'@`  @_�@_\)@^��@]@]`B@]?}@\��@\�@\Z@\(�@[��@[ƨ@[33@[@Z�H@Z�!@Z^5@Y�@Yx�@YX@YG�@YG�@Y7L@Y&�@Y�@Y%@Y%@X��@X�9@X��@XbN@X �@W|�@W;d@W�@V�y@VV@Up�@T��@Tj@TI�@S��@Sƨ@S33@R�@Rn�@R-@Q�@Q�7@P�9@Ol�@O
=@N��@NV@M��@Mp�@L��@L�D@Lj@L9X@K�
@Kƨ@K�F@K��@KS�@Ko@J��@Jn�@J=q@J-@I��@I�#@I�^@Ihs@I�@I%@H�`@H1'@Gl�@G\)@GK�@G+@G
=@G
=@F��@F��@F�y@F��@F�+@Fff@F$�@E��@E�h@E`B@E/@D��@D��@D�j@D�@C�F@B�@B^5@BM�@BJ@A�@A�#@A�7@AX@A�@@��@@��@@��@@�@@A�@?�@>�@>V@>$�@=�@=�-@=@=��@=`B@=V@<��@<��@<Z@<(�@<1@;��@;�@;dZ@;33@;@:��@:�!@:��@:M�@:J@9��@9hs@9G�@9�@8r�@7�P@7|�@7|�@7+@6ȴ@6�+@6V@6V@5�@5@5�h@5O�@4�/@4I�@4(�@41@3��@3�
@3��@3�@333@2��@2�\@2^5@1��@0�`@0r�@0A�@01'@0b@/�;@/�@/�P@/K�@/+@.��@.�@.��@.�+@.v�@.5?@-�-@-/@,�/@,9X@+�m@+ƨ@+�F@+t�@+o@*��@*��@*�!@*��@*�\@)��@)%@(�9@(�9@(�9@(��@(�u@(r�@(b@'��@'�w@'|�@'l�@'\)@'K�@';d@&�R@&5?@&{@&{@&@%�h@%/@$��@$�@$��@$�j@$�j@$�@$z�@$j@$I�@$�@#��@#ƨ@#��@#��@#S�@#C�@#o@"�@#@"�@"�H@"�@"��@"��@"^5@"-@!��@!��@!x�@!&�@ ��@ �u@ bN@ A�@ 1'@   @�w@�P@\)@\)@;d@;d@+@��@�@��@��@��@��@E�@�T@p�@?}@��@�/@�@��@9X@(�@��@t�@dZ@S�@C�@��@��@��@�\@~�@n�@n�@^5@=q@-@�@J@J@�@��@�7@x�@G�@%@�`@��@�u@1'@�@��@��@�w@�@�@�@\)@
=@ȴ@��@ff@$�@��@O�@�@V@V@��@��@�@�/@�@��@j@�@�
@��@�@S�@"�@@�H@��@��@n�@^5@^5@^5@M�@-@�@J@J@�@��@�^@��@x�@X@X@&�@%@�`@Ĝ@Ĝ@�9@�9@��@�u@�@r�@Q�@Q�@1'@  @�@��@|�@+@
=@�@��@ff@5?@{@�@�-@�h@�@`B@O�@/@/@V@�/@�j@��@I�@�@�@1@��@�m@�
@�F@��@��@dZ@"�@
��@
�\@
^5@
J@	�@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�?}A�I�A�K�A�K�A�K�A�K�A�M�A�Q�A�S�A�O�A�O�A�I�A�G�A�C�A�;dA�A�A�I�A�S�A�M�A��A��A�A���A���A���A���A���A��A��TA���A��9A�~�A�
=A��HA��!A��\A�JA��HA��A�G�A�z�A�jA� �A��#A�VA���A���A�I�A�A�A�  A���A�%A��;A�M�A�A��HA�M�A���A���A��A�-A��FA�33A�VA�VA��/A� �A���A��-A���A�VA�^5A�
=A�$�A�~�A���A�~�A�C�A�bNA��wA�A�A��9A���A�G�A��A�&�A���A�x�A�~�A�bNA��A�9XA��!A��uA�A�A��RA���A��A�&�A�M�A�;dA�x�A��7A���A~bNA{/Ay�wAxAvA�Atv�Ap��Al��Aj��Ai�FAh�Ah�Agp�Af1AeVAc�FAa��Aa%A_�;A_oA^v�A]�;AZ��AYdZAX�AV�AT��ATVAS��AQ�mAP�`AP(�AO�-AN��AN-AM��AMt�AM�AL{AK%AI�7AG��AGl�AFbNAE��AD�AC�AB�9AA��A@ȴA@9XA>�!A=�^A=VA;�
A;O�A;K�A;G�A;
=A:r�A:�A:�A9A7&�A61'A5�FA5+A4v�A3A3;dA2��A29XA1l�A1"�A0��A0jA/�-A-��A,~�A+��A+&�A*-A)O�A(��A(A�A&~�A%�mA$�A$1A#t�A"��A!�A!C�A A�HA��AA�A`BA��A-A�A�A�`AA�A�A�DA�FA&�A��A�+A�FA�9Av�A�A�A��A��A��A&�AA�A�A��A�+A1'A��A�AK�A
I�A	�A	S�AQ�A��A`BA�`A�#A
=A1A�uAhsA ĜA J@�ƨ@�M�@��7@��/@�  @�dZ@���@��!@�{@��^@���@�"�@�J@�Ĝ@���@��@�+@�Z@�X@睲@��@�@�7L@��
@�@��@�M�@��@��@���@�bN@�o@޸R@���@�bN@�X@��
@�n�@�ƨ@�o@��@щ7@�bN@Ͼw@��y@ΰ!@��T@���@�t�@�J@ɑh@�G�@���@Ǿw@��@Ų-@��m@�E�@��@�dZ@�n�@�K�@�{@��@��m@���@��@�`B@��@�j@���@�@�ff@���@�|�@�5?@��@���@�r�@�j@���@�C�@�@�n�@��@���@�X@�z�@�dZ@�^5@��@�b@��@��F@��@��@�|�@�t�@�33@���@�hs@�\)@���@�E�@��-@���@��9@��u@��u@��D@��@�bN@�Q�@�(�@��m@���@��!@�=q@��@�@���@�?}@�%@���@��@���@�r�@�A�@�33@��h@���@���@�V@�(�@���@�
=@�^5@�@��#@��h@�/@���@���@��u@�Q�@�(�@�  @��@�5?@���@��/@�I�@��
@�\)@���@�^5@��@���@�@���@�G�@���@��@��@��
@���@�t�@�+@��@��H@��H@���@��R@���@���@��#@��@�p�@�hs@�hs@�X@�/@��@��@�V@���@�z�@�bN@�bN@�bN@�I�@�9X@�1'@�9X@�(�@��@���@���@���@���@�t�@�\)@�C�@��@�o@��H@���@��\@�~�@�ff@�V@�$�@��-@��@�hs@�G�@�%@�Ĝ@��u@�z�@�Q�@�(�@�1@�;@�@|�@~�y@~V@}@|��@|z�@|�@{��@{dZ@{@z~�@z^5@z�@y7L@x �@x  @w��@w�@w|�@wl�@wK�@v��@u�@u�@u�@tZ@s��@t1@s�
@s��@sC�@so@r�!@rM�@r�@q�@q�7@qhs@q�@pĜ@pr�@o��@o�P@o;d@n�R@nff@nff@n5?@m�@mO�@m/@l�/@l��@lZ@lI�@l�@k�F@k@j�\@j-@i�@i�@i�@i�#@i��@i%@h�u@h�@h�@hbN@h �@g�@g�;@g�@g|�@gK�@f��@f��@f@eV@d��@d�D@dz�@dI�@dI�@d9X@d�@c��@c��@ct�@c33@b�H@a��@a�^@a7L@a�@`��@`�@`bN@`A�@`1'@`  @_�@_\)@^��@]@]`B@]?}@\��@\�@\Z@\(�@[��@[ƨ@[33@[@Z�H@Z�!@Z^5@Y�@Yx�@YX@YG�@YG�@Y7L@Y&�@Y�@Y%@Y%@X��@X�9@X��@XbN@X �@W|�@W;d@W�@V�y@VV@Up�@T��@Tj@TI�@S��@Sƨ@S33@R�@Rn�@R-@Q�@Q�7@P�9@Ol�@O
=@N��@NV@M��@Mp�@L��@L�D@Lj@L9X@K�
@Kƨ@K�F@K��@KS�@Ko@J��@Jn�@J=q@J-@I��@I�#@I�^@Ihs@I�@I%@H�`@H1'@Gl�@G\)@GK�@G+@G
=@G
=@F��@F��@F�y@F��@F�+@Fff@F$�@E��@E�h@E`B@E/@D��@D��@D�j@D�@C�F@B�@B^5@BM�@BJ@A�@A�#@A�7@AX@A�@@��@@��@@��@@�@@A�@?�@>�@>V@>$�@=�@=�-@=@=��@=`B@=V@<��@<��@<Z@<(�@<1@;��@;�@;dZ@;33@;@:��@:�!@:��@:M�@:J@9��@9hs@9G�@9�@8r�@7�P@7|�@7|�@7+@6ȴ@6�+@6V@6V@5�@5@5�h@5O�@4�/@4I�@4(�@41@3��@3�
@3��@3�@333@2��@2�\@2^5@1��@0�`@0r�@0A�@01'@0b@/�;@/�@/�P@/K�@/+@.��@.�@.��@.�+@.v�@.5?@-�-@-/@,�/@,9X@+�m@+ƨ@+�F@+t�@+o@*��@*��@*�!@*��@*�\@)��@)%@(�9@(�9@(�9@(��@(�u@(r�@(b@'��@'�w@'|�@'l�@'\)@'K�@';d@&�R@&5?@&{@&{@&@%�h@%/@$��@$�@$��@$�j@$�j@$�@$z�@$j@$I�@$�@#��@#ƨ@#��@#��@#S�@#C�@#o@"�@#@"�@"�H@"�@"��@"��@"^5@"-@!��@!��@!x�@!&�@ ��@ �u@ bN@ A�@ 1'@   @�w@�P@\)@\)@;d@;d@+@��@�@��@��@��@��@E�@�T@p�@?}@��@�/@�@��@9X@(�@��@t�@dZ@S�@C�@��@��@��@�\@~�@n�@n�@^5@=q@-@�@J@J@�@��@�7@x�@G�@%@�`@��@�u@1'@�@��@��@�w@�@�@�@\)@
=@ȴ@��@ff@$�@��@O�@�@V@V@��@��@�@�/@�@��@j@�@�
@��@�@S�@"�@@�H@��@��@n�@^5@^5@^5@M�@-@�@J@J@�@��@�^@��@x�@X@X@&�@%@�`@Ĝ@Ĝ@�9@�9@��@�u@�@r�@Q�@Q�@1'@  @�@��@|�@+@
=@�@��@ff@5?@{@�@�-@�h@�@`B@O�@/@/@V@�/@�j@��@I�@�@�@1@��@�m@�
@�F@��@��@dZ@"�@
��@
�\@
^5@
J@	�@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B:^B:^B9XB9XB8RB8RB7LB7LB7LB7LB6FB6FB5?B5?B49B33B49B5?B5?B49B1'B1'B1'B2-B33B49B49B49B5?B<jBD�BI�BN�BXBiyBiyBk�BdZB[#BR�BN�BL�BM�BH�B5?B'�B$�B+B<jBP�BVBS�BcTBw�B�B|�Bz�Br�BiyBbNB^5BT�BK�BG�B?}B49B-B#�B�BVB��B�B�`B�BB��BÖB�^B�!B�B�B��B�=BaHBN�BH�BA�B)�B�B�B1B�`B�qB�B��B�%B�Bx�BjB]/BN�B6FB#�B�B
=B��B�B�^B�-B��B��B�\Bx�B\)BP�BK�BM�BH�BD�B7LB33B.B�B,B,B&�B!�B�B{B	7BB��B�B�B�B�B�`B�BB�5B�)B�#B�B�B�)B�B�B��B��BɺBǮBB��BÖB�qB�^B�FB�9B��B��B��B��B��B��B��B��B��B��B��B��B�DB�B}�B|�Bz�Bx�Bv�Bt�Bq�Bn�Bl�Bm�Bk�BhsBbNB]/BZBW
BT�BP�BN�BL�BG�BE�BC�BA�B?}B<jB9XB6FB2-B,B"�B�B�B�BoBbBVBPBJB
=B1B+B%BBBBB  B  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�yB
�sB
�fB
�`B
�TB
�HB
�HB
�HB
�BB
�;B
�;B
�;B
�;B
�;B
�5B
�5B
�5B
�/B
�5B
�)B
�/B
�/B
�)B
�#B
�)B
�B
�#B
�#B
�B
�B
�#B
�#B
�B
�#B
�#B
�#B
�#B
�#B
�)B
�B
�B
�#B
�/B
�)B
�/B
�BB
�BB
�;B
�HB
�HB
�NB
�ZB
�NB
�TB
�NB
�`B
�mB
�mB
�sB
�sB
�yB
�B
�B
�B
�B
��B
��B
��BBB+BPBhBoBoB{B{BuB{B{B�B�B�B�B�B�B�B!�B!�B!�B#�B%�B$�B$�B'�B+B-B33B7LB7LB9XB:^B;dB=qB>wB@�BA�BG�BN�BP�BR�BT�BYBYBZBZBZBYBZBYBYBYBXB\)B^5B_;B`BBaHBdZBe`BgmBhsBiyBjBiyBm�Bw�B|�B}�B~�B�B�%B�1B�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�-B�LB�dB�wBĜBǮB��B��B��B��B��B��B�
B�#B�/B�BB�NB�`B�mB�sB�sB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBB+B1B
=BDBJBPBVBbBbBoB{B�B�B�B�B�B�B�B�B �B"�B%�B&�B'�B)�B)�B,B,B-B.B1'B33B5?B8RB:^B<jB>wB?}BA�BB�BC�BD�BH�BN�BN�BP�BQ�BQ�BR�BS�BT�BZB\)B]/BaHBbNBbNBcTBdZBe`BgmBiyBjBk�Bl�Bn�Bo�Bp�Br�Br�Bu�Bv�Bw�By�B{�Bz�B{�B|�B� B� B�B�B�B�B�B�%B�7B�DB�JB�PB�PB�PB�PB�VB�oB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�9B�?B�?B�?B�?B�FB�LB�LB�XB�jB�qB�qB�wB��B��B��BBBÖBĜBŢBƨBǮBȴBɺBɺB��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�
B�B�B�B�#B�)B�)B�/B�;B�HB�`B�`B�fB�mB�mB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBB%BB+B+B+B1B1B	7B1B	7B	7BJBPBPBPBVBVBVB\B\BbBbBhBhBhBoBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B!�B"�B#�B"�B#�B&�B&�B&�B&�B&�B'�B'�B'�B(�B(�B)�B)�B)�B)�B)�B+B+B-B-B.B/B/B/B0!B0!B1'B1'B1'B1'B1'B33B33B49B49B49B49B5?B5?B5?B5?B5?B6FB6FB7LB6FB6FB8RB8RB8RB8RB9XB9XB:^B:^B:^B;dB;dB;dB;dB<jB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�BA�BA�BB�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BE�BF�BG�BH�BH�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BM�BL�BM�BN�BN�BO�BN�BO�BO�BO�BP�BQ�BQ�BQ�BQ�BQ�BR�BR�BQ�BR�BS�BS�BS�BS�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BW
BXBXBYBYBYBYBZBYBZB[#BZBZBZB[#B[#B[#B[#B\)B\)B[#B\)B\)B\)B\)B]/B]/B\)B]/B]/B]/B^5B^5B^5B^5B^5B^5B^5B^5B_;B_;B_;B_;B_;B_;B`BB`BB`BBaHBaHBaHBaHBbNBbNBbNBbNBcTBcTBcTBcTBdZBdZBdZBdZBdZBe`Be`Be`Be`Be`BffBe`BffBffBffBffBgmBgmBgmBhsBiyBiyBiy4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B:^B:^B9XB9XB8RB8RB7LB7LB7LB7LB6FB6FB5?B5?B49B33B49B5?B5?B49B1'B1'B1'B2-B33B49B49B49B5?B<jBD�BI�BN�BXBiyBiyBk�BdZB[#BR�BN�BL�BM�BH�B5?B'�B$�B+B<jBP�BVBS�BcTBw�B�B|�Bz�Br�BiyBbNB^5BT�BK�BG�B?}B49B-B#�B�BVB��B�B�`B�BB��BÖB�^B�!B�B�B��B�=BaHBN�BH�BA�B)�B�B�B1B�`B�qB�B��B�%B�Bx�BjB]/BN�B6FB#�B�B
=B��B�B�^B�-B��B��B�\Bx�B\)BP�BK�BM�BH�BD�B7LB33B.B�B,B,B&�B!�B�B{B	7BB��B�B�B�B�B�`B�BB�5B�)B�#B�B�B�)B�B�B��B��BɺBǮBB��BÖB�qB�^B�FB�9B��B��B��B��B��B��B��B��B��B��B��B��B�DB�B}�B|�Bz�Bx�Bv�Bt�Bq�Bn�Bl�Bm�Bk�BhsBbNB]/BZBW
BT�BP�BN�BL�BG�BE�BC�BA�B?}B<jB9XB6FB2-B,B"�B�B�B�BoBbBVBPBJB
=B1B+B%BBBBB  B  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�yB
�sB
�fB
�`B
�TB
�HB
�HB
�HB
�BB
�;B
�;B
�;B
�;B
�;B
�5B
�5B
�5B
�/B
�5B
�)B
�/B
�/B
�)B
�#B
�)B
�B
�#B
�#B
�B
�B
�#B
�#B
�B
�#B
�#B
�#B
�#B
�#B
�)B
�B
�B
�#B
�/B
�)B
�/B
�BB
�BB
�;B
�HB
�HB
�NB
�ZB
�NB
�TB
�NB
�`B
�mB
�mB
�sB
�sB
�yB
�B
�B
�B
�B
��B
��B
��BBB+BPBhBoBoB{B{BuB{B{B�B�B�B�B�B�B�B!�B!�B!�B#�B%�B$�B$�B'�B+B-B33B7LB7LB9XB:^B;dB=qB>wB@�BA�BG�BN�BP�BR�BT�BYBYBZBZBZBYBZBYBYBYBXB\)B^5B_;B`BBaHBdZBe`BgmBhsBiyBjBiyBm�Bw�B|�B}�B~�B�B�%B�1B�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�-B�LB�dB�wBĜBǮB��B��B��B��B��B��B�
B�#B�/B�BB�NB�`B�mB�sB�sB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBB+B1B
=BDBJBPBVBbBbBoB{B�B�B�B�B�B�B�B�B �B"�B%�B&�B'�B)�B)�B,B,B-B.B1'B33B5?B8RB:^B<jB>wB?}BA�BB�BC�BD�BH�BN�BN�BP�BQ�BQ�BR�BS�BT�BZB\)B]/BaHBbNBbNBcTBdZBe`BgmBiyBjBk�Bl�Bn�Bo�Bp�Br�Br�Bu�Bv�Bw�By�B{�Bz�B{�B|�B� B� B�B�B�B�B�B�%B�7B�DB�JB�PB�PB�PB�PB�VB�oB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�9B�?B�?B�?B�?B�FB�LB�LB�XB�jB�qB�qB�wB��B��B��BBBÖBĜBŢBƨBǮBȴBɺBɺB��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�
B�B�B�B�#B�)B�)B�/B�;B�HB�`B�`B�fB�mB�mB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBB%BB+B+B+B1B1B	7B1B	7B	7BJBPBPBPBVBVBVB\B\BbBbBhBhBhBoBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B!�B"�B#�B"�B#�B&�B&�B&�B&�B&�B'�B'�B'�B(�B(�B)�B)�B)�B)�B)�B+B+B-B-B.B/B/B/B0!B0!B1'B1'B1'B1'B1'B33B33B49B49B49B49B5?B5?B5?B5?B5?B6FB6FB7LB6FB6FB8RB8RB8RB8RB9XB9XB:^B:^B:^B;dB;dB;dB;dB<jB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�BA�BA�BB�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BE�BF�BG�BH�BH�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BM�BL�BM�BN�BN�BO�BN�BO�BO�BO�BP�BQ�BQ�BQ�BQ�BQ�BR�BR�BQ�BR�BS�BS�BS�BS�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BW
BXBXBYBYBYBYBZBYBZB[#BZBZBZB[#B[#B[#B[#B\)B\)B[#B\)B\)B\)B\)B]/B]/B\)B]/B]/B]/B^5B^5B^5B^5B^5B^5B^5B^5B_;B_;B_;B_;B_;B_;B`BB`BB`BBaHBaHBaHBaHBbNBbNBbNBbNBcTBcTBcTBcTBdZBdZBdZBdZBdZBe`Be`Be`Be`Be`BffBe`BffBffBffBffBgmBgmBgmBhsBiyBiyBiy4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220404090104                              AO  ARCAADJP                                                                    20220404090104    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220404090104  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220404090104  QCF$                G�O�G�O�G�O�8000            