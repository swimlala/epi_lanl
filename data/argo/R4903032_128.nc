CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-12-25T10:01:09Z creation      
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
resolution        =���   axis      Z        t  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211225100109  20211225100109  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @٬�eN1   @٬֦�΂@<��+�cɁ$�/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D���D�<�Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D���D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@�Q�A(�A:�\A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�
Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�>D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�>DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD���D��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��D��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�{�D���D��RD�;�D�{�D��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��D�5D�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��D�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDظRD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݵD��RD�8RD�xRD޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�;�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A��A��A��A�~�A��A��A��+A��+A��7A��DA��7A��7A��DA��\A��\A��hA��hA��uA��uA��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�l�A�5?A�ZA�A��^A��HA��A���A�33A���A��/A���A�1A���A�9XA��A�ffA�5?A�"�A��/A�Q�A�bA���A��A��jA�Q�A�A�A�ffA���A�33A���A��yA���A��;A�%A�M�A���A��A��`A���A��-A��!A��A�E�A��!A��A��A��mA���A�r�A�oA��A�&�A~ȴA|Q�Az�Ay�7Av�/As�Aq
=Ao�hAm�-Aln�Ak�hAjI�AiAhbNAgƨAgG�Ag�AgAf�jAfJAe��AeS�Ad�Ad�uAc��Ab�jAbAal�Aa
=A`r�A^��A]�A]�A\�9A\AZbNAY�AX�AX1'AW/AU�wAS��ASK�AR��AQ�-AP{AOp�AO33AN�9AM��AMS�AM
=AL��AL��AK��AJ�yAIG�AH�AG�AFn�AE�ADbAC"�ABĜAB9XAA�A@n�A?�A>JA=��A=�A< �A;�A:�!A9�TA9�A9&�A8~�A7�A6�A5S�A4��A3�7A2�HA2I�A1XA0bNA/�PA/\)A/
=A.�9A.-A-\)A,��A+�TA+"�A*r�A)�wA)"�A(M�A'p�A%��A$I�A#K�A"��A"5?A ��A �AA�A�^AdZAI�A|�AG�A
=A�
A�7A�yA�+At�AbNA�A�A�A�RAQ�AO�Ar�AoA�A��AjA��A`BA33A�AS�A	�hA��A�RAr�AZAQ�A��A|�A`BA��A�An�A �R@��@���@��y@�V@���@���@��9@��P@��H@��T@�j@��@��@�A�@�R@�x�@�j@��;@ꟾ@�=q@���@�dZ@�b@�C�@�
=@�\@�X@��/@�1'@��@�{@�?}@�z�@�1@�t�@���@�x�@У�@��@϶F@��H@�p�@�b@�\)@�V@�I�@��@��y@��/@Å@���@�@�n�@���@��;@�l�@�K�@�v�@��@��`@��@���@���@�|�@�K�@���@��@�I�@�"�@���@�^5@��@�%@�r�@���@�M�@��9@�j@�ƨ@�t�@�n�@�@��#@�hs@��j@�9X@��;@��y@�-@��#@���@�p�@�/@��9@�bN@�  @���@�l�@�;d@��y@���@�V@��T@���@��-@���@���@��@���@� �@�33@��+@�=q@�@�@�x�@�%@�Z@��@�\)@�C�@�+@���@��+@��+@�~�@�^5@�E�@��@��@��D@�I�@�  @���@��m@��w@�|�@��@���@�=q@���@���@�`B@���@�1'@�\)@�o@��y@��R@��\@�^5@��T@�7L@��`@��@��@�33@��R@�n�@�@���@�x�@�p�@�p�@�?}@���@���@�r�@�Q�@�1'@�1@��@�\)@�
=@���@���@�ff@�E�@�=q@��@���@��@�p�@�G�@���@��@�(�@���@�|�@�\)@�o@�ȴ@�$�@��@��7@�`B@���@��@�Q�@�1@��;@��w@���@��P@��@�|�@�l�@�dZ@�S�@��@�~�@�E�@��@��T@���@�@��h@�O�@��@��/@�bN@�(�@��@�b@�@;d@~v�@~@}��@}O�@|j@|1@{�m@{�@{@z��@z�!@zM�@y�#@yx�@yG�@y%@xĜ@xQ�@xA�@x �@x  @w�;@w�w@w|�@w
=@v�+@v5?@u�@u��@u?}@t�@t�D@tz�@t(�@s�m@s��@sC�@s@r�@qx�@p�9@pQ�@p  @o�;@ol�@n�@n5?@m�-@m��@m�@lz�@lZ@lZ@lI�@k�m@k�@k33@ko@j�\@j-@jJ@i�@i��@i��@iX@i�@i%@hĜ@h�u@h�@hbN@g�@g�@g|�@g;d@g�@f�@fV@e�T@e�h@e?}@d�@d��@d��@dI�@c�@c@b=q@a�@a��@ax�@a&�@`�9@`r�@`1'@_�@_|�@_�@^��@^E�@]@\�@\j@\(�@[�F@[t�@[dZ@[S�@Z�H@Z�!@Z^5@Z=q@Z-@Z�@Y�#@YG�@Y%@XĜ@X�@XA�@W�@W�w@W�P@W+@V�y@V�@V��@VV@U�T@U�@U�@U�@U`B@U/@T�/@T��@TI�@S��@S�F@S�@S"�@R��@R~�@Rn�@RM�@Q��@Qhs@Q�@P�`@P��@P�u@PA�@Pb@O��@O�@O�P@O|�@O�@N�@Nȴ@N��@NV@N@M�-@M`B@L��@L�j@L��@Lj@L(�@K��@K@J�\@Jn�@J^5@J-@J�@I�#@I��@IG�@Hr�@H �@G�;@G;d@F�y@Fv�@F5?@F{@E�@E�T@E@E/@D��@Dj@DI�@D1@C�
@C��@C"�@C@C@B�@B��@B~�@B�@A�@A��@A��@A�7@AG�@A%@@��@@�9@@ �@?��@?
=@>ȴ@>v�@>{@=��@=@=�@=/@<��@<��@<��@<�D@<z�@<Z@<9X@<(�@;��@;�F@;��@;�@;dZ@;33@:��@:^5@:M�@:�@:�@:�@:J@9��@9��@9�@9%@8��@8�@7�w@6�y@6�+@6v�@6v�@6ff@6E�@6@5��@5�@5`B@5O�@5?}@5V@4�/@4��@4I�@4(�@4�@3�m@3ƨ@3S�@2�@2�!@2�@1��@1�#@1��@1��@1x�@1&�@0��@0r�@0A�@0b@/�w@/�@.ȴ@.v�@.V@-�@-�-@-p�@-/@,��@,�/@,��@,Z@,(�@+��@+�F@+t�@+"�@*�!@*~�@*^5@*J@)�^@)��@)hs@)7L@(��@(��@(Ĝ@(�9@(�u@(�u@(�@(�@(�@(r�@(1'@'�;@'�P@'l�@'K�@'K�@'+@&ȴ@&�+@&V@&E�@&5?@&$�@%�@%�h@%O�@%�@$��@$��@$�@$z�@$Z@$(�@#ƨ@#��@#t�@#S�@#C�@#"�@"��@"�!@"n�@"-@!��@!��@!�#@!�^@!��@!��@!�7@!�7@!�7@!x�@!X@!7L@ ��@ �9@ Q�@ b@ b@�@K�@
=@ȴ@��@ff@5?@{@�@��@O�@�/@�j@�@�D@j@Z@9X@(�@1@�
@��@��@�@t�@t�@dZ@@M�@�@J@�@�^@hs@�@%@Ĝ@��@Q�@ �@�@\)@�R@��@��@�+@ff@{@@�@��@�h@�@p�@p�@O�@?}@/@��@�@z�@9X@�m@ƨ@�@"�@�!@-@�@��@hs@G�@�@Ĝ@�u@bN@A�@1'@b@  @�;@�;@��@��@�w@�@�P@;d@+@�y@ȴ@�R@��@��@�+@v�@V@E�@{@�@�T@�-@`B@?}@�/@�D@j@Z@9X@(�@�@��@�
@�F@��@��@��@��@��@��@�@�@S�@"�@o@@
�@
�H@
��@
�!@
�!@
�!@
~�@
n�@
^5@
M�@
-@	�@	�@	��@	��@	X@	&�@	%@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�r�A��A��A��A�~�A��A��A��+A��+A��7A��DA��7A��7A��DA��\A��\A��hA��hA��uA��uA��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�l�A�5?A�ZA�A��^A��HA��A���A�33A���A��/A���A�1A���A�9XA��A�ffA�5?A�"�A��/A�Q�A�bA���A��A��jA�Q�A�A�A�ffA���A�33A���A��yA���A��;A�%A�M�A���A��A��`A���A��-A��!A��A�E�A��!A��A��A��mA���A�r�A�oA��A�&�A~ȴA|Q�Az�Ay�7Av�/As�Aq
=Ao�hAm�-Aln�Ak�hAjI�AiAhbNAgƨAgG�Ag�AgAf�jAfJAe��AeS�Ad�Ad�uAc��Ab�jAbAal�Aa
=A`r�A^��A]�A]�A\�9A\AZbNAY�AX�AX1'AW/AU�wAS��ASK�AR��AQ�-AP{AOp�AO33AN�9AM��AMS�AM
=AL��AL��AK��AJ�yAIG�AH�AG�AFn�AE�ADbAC"�ABĜAB9XAA�A@n�A?�A>JA=��A=�A< �A;�A:�!A9�TA9�A9&�A8~�A7�A6�A5S�A4��A3�7A2�HA2I�A1XA0bNA/�PA/\)A/
=A.�9A.-A-\)A,��A+�TA+"�A*r�A)�wA)"�A(M�A'p�A%��A$I�A#K�A"��A"5?A ��A �AA�A�^AdZAI�A|�AG�A
=A�
A�7A�yA�+At�AbNA�A�A�A�RAQ�AO�Ar�AoA�A��AjA��A`BA33A�AS�A	�hA��A�RAr�AZAQ�A��A|�A`BA��A�An�A �R@��@���@��y@�V@���@���@��9@��P@��H@��T@�j@��@��@�A�@�R@�x�@�j@��;@ꟾ@�=q@���@�dZ@�b@�C�@�
=@�\@�X@��/@�1'@��@�{@�?}@�z�@�1@�t�@���@�x�@У�@��@϶F@��H@�p�@�b@�\)@�V@�I�@��@��y@��/@Å@���@�@�n�@���@��;@�l�@�K�@�v�@��@��`@��@���@���@�|�@�K�@���@��@�I�@�"�@���@�^5@��@�%@�r�@���@�M�@��9@�j@�ƨ@�t�@�n�@�@��#@�hs@��j@�9X@��;@��y@�-@��#@���@�p�@�/@��9@�bN@�  @���@�l�@�;d@��y@���@�V@��T@���@��-@���@���@��@���@� �@�33@��+@�=q@�@�@�x�@�%@�Z@��@�\)@�C�@�+@���@��+@��+@�~�@�^5@�E�@��@��@��D@�I�@�  @���@��m@��w@�|�@��@���@�=q@���@���@�`B@���@�1'@�\)@�o@��y@��R@��\@�^5@��T@�7L@��`@��@��@�33@��R@�n�@�@���@�x�@�p�@�p�@�?}@���@���@�r�@�Q�@�1'@�1@��@�\)@�
=@���@���@�ff@�E�@�=q@��@���@��@�p�@�G�@���@��@�(�@���@�|�@�\)@�o@�ȴ@�$�@��@��7@�`B@���@��@�Q�@�1@��;@��w@���@��P@��@�|�@�l�@�dZ@�S�@��@�~�@�E�@��@��T@���@�@��h@�O�@��@��/@�bN@�(�@��@�b@�@;d@~v�@~@}��@}O�@|j@|1@{�m@{�@{@z��@z�!@zM�@y�#@yx�@yG�@y%@xĜ@xQ�@xA�@x �@x  @w�;@w�w@w|�@w
=@v�+@v5?@u�@u��@u?}@t�@t�D@tz�@t(�@s�m@s��@sC�@s@r�@qx�@p�9@pQ�@p  @o�;@ol�@n�@n5?@m�-@m��@m�@lz�@lZ@lZ@lI�@k�m@k�@k33@ko@j�\@j-@jJ@i�@i��@i��@iX@i�@i%@hĜ@h�u@h�@hbN@g�@g�@g|�@g;d@g�@f�@fV@e�T@e�h@e?}@d�@d��@d��@dI�@c�@c@b=q@a�@a��@ax�@a&�@`�9@`r�@`1'@_�@_|�@_�@^��@^E�@]@\�@\j@\(�@[�F@[t�@[dZ@[S�@Z�H@Z�!@Z^5@Z=q@Z-@Z�@Y�#@YG�@Y%@XĜ@X�@XA�@W�@W�w@W�P@W+@V�y@V�@V��@VV@U�T@U�@U�@U�@U`B@U/@T�/@T��@TI�@S��@S�F@S�@S"�@R��@R~�@Rn�@RM�@Q��@Qhs@Q�@P�`@P��@P�u@PA�@Pb@O��@O�@O�P@O|�@O�@N�@Nȴ@N��@NV@N@M�-@M`B@L��@L�j@L��@Lj@L(�@K��@K@J�\@Jn�@J^5@J-@J�@I�#@I��@IG�@Hr�@H �@G�;@G;d@F�y@Fv�@F5?@F{@E�@E�T@E@E/@D��@Dj@DI�@D1@C�
@C��@C"�@C@C@B�@B��@B~�@B�@A�@A��@A��@A�7@AG�@A%@@��@@�9@@ �@?��@?
=@>ȴ@>v�@>{@=��@=@=�@=/@<��@<��@<��@<�D@<z�@<Z@<9X@<(�@;��@;�F@;��@;�@;dZ@;33@:��@:^5@:M�@:�@:�@:�@:J@9��@9��@9�@9%@8��@8�@7�w@6�y@6�+@6v�@6v�@6ff@6E�@6@5��@5�@5`B@5O�@5?}@5V@4�/@4��@4I�@4(�@4�@3�m@3ƨ@3S�@2�@2�!@2�@1��@1�#@1��@1��@1x�@1&�@0��@0r�@0A�@0b@/�w@/�@.ȴ@.v�@.V@-�@-�-@-p�@-/@,��@,�/@,��@,Z@,(�@+��@+�F@+t�@+"�@*�!@*~�@*^5@*J@)�^@)��@)hs@)7L@(��@(��@(Ĝ@(�9@(�u@(�u@(�@(�@(�@(r�@(1'@'�;@'�P@'l�@'K�@'K�@'+@&ȴ@&�+@&V@&E�@&5?@&$�@%�@%�h@%O�@%�@$��@$��@$�@$z�@$Z@$(�@#ƨ@#��@#t�@#S�@#C�@#"�@"��@"�!@"n�@"-@!��@!��@!�#@!�^@!��@!��@!�7@!�7@!�7@!x�@!X@!7L@ ��@ �9@ Q�@ b@ b@�@K�@
=@ȴ@��@ff@5?@{@�@��@O�@�/@�j@�@�D@j@Z@9X@(�@1@�
@��@��@�@t�@t�@dZ@@M�@�@J@�@�^@hs@�@%@Ĝ@��@Q�@ �@�@\)@�R@��@��@�+@ff@{@@�@��@�h@�@p�@p�@O�@?}@/@��@�@z�@9X@�m@ƨ@�@"�@�!@-@�@��@hs@G�@�@Ĝ@�u@bN@A�@1'@b@  @�;@�;@��@��@�w@�@�P@;d@+@�y@ȴ@�R@��@��@�+@v�@V@E�@{@�@�T@�-@`B@?}@�/@�D@j@Z@9X@(�@�@��@�
@�F@��@��@��@��@��@��@�@�@S�@"�@o@@
�@
�H@
��@
�!@
�!@
�!@
~�@
n�@
^5@
M�@
-@	�@	�@	��@	��@	X@	&�@	%@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�mB�fB�fB�`B�`B�ZB�TB�TB�TB�NB�NB�HB�HB�HB�HB�BB�BB�BB�BB�BB�BB�;B�;B�;B�;B�BB�BB�BB�BB�BB�;B�BB�;B�;B�BB�BB�BB�;B�;B�;B�5B�B��B�7B��B�B�B�ZB�/B�B��BŢB�XB�!B�B��B��B�uB�Bv�BaHBXBR�BK�BA�B;dB49B&�B{B1B��B�BB��BǮB�jB�B��B��B�7Bz�BhsBYBD�B<jB/B$�B�BoB	7BB��B��B�HB��BɺB�XB�'B��B��B�JBx�Bo�Be`B^5BXBVBP�BN�BM�BL�BK�BK�BJ�BI�BG�BF�BF�BF�BF�BE�BA�B>wB;dB8RB.B �B�B�B�BbB
=B%BB  B��B�B�B�sB�NB�#B�
B�#B�BB�#B�B�
B�;B�BB�/B�
B��BǮBŢB�}B�jB�FB�'B�B�B��B��B��B��B��B��B�hB�\B�PB�DB�7B�+B�B|�Bw�Bp�Bk�BffBaHB^5BXBS�BM�BL�BK�BI�BG�BD�BA�B>wB;dB8RB5?B49B1'B+B"�B�B�B�B{BbBPBPBDB+B%BB  B
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
�yB
�mB
�`B
�TB
�NB
�NB
�HB
�BB
�;B
�)B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ŢB
ŢB
ŢB
ĜB
ĜB
ÖB
ÖB
��B
��B
�}B
�wB
�qB
�jB
�XB
�XB
�RB
�LB
�LB
�FB
�FB
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�9B
�3B
�?B
�?B
�?B
�?B
�?B
�3B
�RB
�RB
�RB
�RB
�RB
�XB
�dB
�dB
�dB
�wB
�}B
�wB
�}B
ÖB
ĜB
ŢB
ƨB
ŢB
ƨB
��B
��B
��B
��B
��B
�
B
�B
�B
�B
�B
�B
�#B
�)B
�HB
�ZB
�`B
�fB
�mB
�B
�B
�B
�B
��B
��B
��B
��BBBB+B	7BDBJBhB{B�B�B�B�B�B�B�B �B!�B"�B#�B%�B&�B(�B)�B)�B)�B)�B+B-B2-B7LB:^B;dB<jB>wB?}BA�BE�BG�BK�BK�BL�BO�BP�BP�BP�BP�BQ�BS�BYB]/B_;BaHBaHBaHBbNBdZBffBhsBk�Bl�Bm�Bo�Bs�Bw�B|�B~�B� B�B�B�B�%B�=B�JB�VB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�FB�RB�^B�jB�wB�}B�}BBÖBŢBŢBƨBȴB��B��B��B��B��B��B�
B�)B�/B�BB�HB�ZB�fB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B  BBBBB+B1BPB\B\B\BbBuB�B�B�B�B �B"�B#�B$�B'�B(�B(�B+B-B.B0!B1'B2-B5?B5?B6FB6FB7LB7LB8RB;dB=qB>wB?}B@�BB�BD�BF�BF�BH�BI�BJ�BK�BL�BO�BR�BVBXBYBYB[#B]/B_;BaHBaHBaHBcTBcTBcTBcTBdZBe`BffBgmBhsBjBjBk�Bl�Bl�Bm�Bn�Bo�Bp�Bq�Bq�Bq�Bs�Bt�Bu�Bv�Bw�Bx�Bz�B|�B~�B� B�B�B�B�B�B�%B�7B�=B�DB�DB�JB�VB�\B�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�-B�3B�3B�3B�3B�9B�?B�?B�FB�LB�RB�XB�^B�dB�jB�jB�jB�qB�wB�}B��B��BÖBĜBŢBƨBƨBƨBƨBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�B�B�B�B�#B�#B�#B�#B�#B�/B�5B�5B�;B�;B�BB�BB�HB�HB�HB�HB�NB�NB�TB�ZB�ZB�ZB�`B�fB�fB�mB�mB�sB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBBBBB%B%B+B+B1B	7B	7B
=B
=BDBDBDBJBJBJBPBPBVBVBVB\B\BbBbBhBhBoBoBoBuBuBuBuB{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B!�B!�B"�B"�B"�B#�B#�B$�B$�B$�B$�B$�B%�B%�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B+B+B,B,B-B-B-B-B.B.B.B/B0!B0!B0!B0!B1'B1'B1'B1'B2-B2-B2-B2-B2-B2-B2-B2-B33B33B33B49B49B49B5?B5?B6FB7LB7LB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB<jB<jB<jB<jB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BF�BF�BG�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�mB�fB�fB�`B�`B�ZB�TB�TB�TB�NB�NB�HB�HB�HB�HB�BB�BB�BB�BB�BB�BB�;B�;B�;B�;B�BB�BB�BB�BB�BB�;B�BB�;B�;B�BB�BB�BB�;B�;B�;B�5B�B��B�7B��B�B�B�ZB�/B�B��BŢB�XB�!B�B��B��B�uB�Bv�BaHBXBR�BK�BA�B;dB49B&�B{B1B��B�BB��BǮB�jB�B��B��B�7Bz�BhsBYBD�B<jB/B$�B�BoB	7BB��B��B�HB��BɺB�XB�'B��B��B�JBx�Bo�Be`B^5BXBVBP�BN�BM�BL�BK�BK�BJ�BI�BG�BF�BF�BF�BF�BE�BA�B>wB;dB8RB.B �B�B�B�BbB
=B%BB  B��B�B�B�sB�NB�#B�
B�#B�BB�#B�B�
B�;B�BB�/B�
B��BǮBŢB�}B�jB�FB�'B�B�B��B��B��B��B��B��B�hB�\B�PB�DB�7B�+B�B|�Bw�Bp�Bk�BffBaHB^5BXBS�BM�BL�BK�BI�BG�BD�BA�B>wB;dB8RB5?B49B1'B+B"�B�B�B�B{BbBPBPBDB+B%BB  B
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
�yB
�mB
�`B
�TB
�NB
�NB
�HB
�BB
�;B
�)B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ŢB
ŢB
ŢB
ĜB
ĜB
ÖB
ÖB
��B
��B
�}B
�wB
�qB
�jB
�XB
�XB
�RB
�LB
�LB
�FB
�FB
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�9B
�3B
�?B
�?B
�?B
�?B
�?B
�3B
�RB
�RB
�RB
�RB
�RB
�XB
�dB
�dB
�dB
�wB
�}B
�wB
�}B
ÖB
ĜB
ŢB
ƨB
ŢB
ƨB
��B
��B
��B
��B
��B
�
B
�B
�B
�B
�B
�B
�#B
�)B
�HB
�ZB
�`B
�fB
�mB
�B
�B
�B
�B
��B
��B
��B
��BBBB+B	7BDBJBhB{B�B�B�B�B�B�B�B �B!�B"�B#�B%�B&�B(�B)�B)�B)�B)�B+B-B2-B7LB:^B;dB<jB>wB?}BA�BE�BG�BK�BK�BL�BO�BP�BP�BP�BP�BQ�BS�BYB]/B_;BaHBaHBaHBbNBdZBffBhsBk�Bl�Bm�Bo�Bs�Bw�B|�B~�B� B�B�B�B�%B�=B�JB�VB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�FB�RB�^B�jB�wB�}B�}BBÖBŢBŢBƨBȴB��B��B��B��B��B��B�
B�)B�/B�BB�HB�ZB�fB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B  BBBBB+B1BPB\B\B\BbBuB�B�B�B�B �B"�B#�B$�B'�B(�B(�B+B-B.B0!B1'B2-B5?B5?B6FB6FB7LB7LB8RB;dB=qB>wB?}B@�BB�BD�BF�BF�BH�BI�BJ�BK�BL�BO�BR�BVBXBYBYB[#B]/B_;BaHBaHBaHBcTBcTBcTBcTBdZBe`BffBgmBhsBjBjBk�Bl�Bl�Bm�Bn�Bo�Bp�Bq�Bq�Bq�Bs�Bt�Bu�Bv�Bw�Bx�Bz�B|�B~�B� B�B�B�B�B�B�%B�7B�=B�DB�DB�JB�VB�\B�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�-B�3B�3B�3B�3B�9B�?B�?B�FB�LB�RB�XB�^B�dB�jB�jB�jB�qB�wB�}B��B��BÖBĜBŢBƨBƨBƨBƨBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�B�B�B�B�#B�#B�#B�#B�#B�/B�5B�5B�;B�;B�BB�BB�HB�HB�HB�HB�NB�NB�TB�ZB�ZB�ZB�`B�fB�fB�mB�mB�sB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBBBBB%B%B+B+B1B	7B	7B
=B
=BDBDBDBJBJBJBPBPBVBVBVB\B\BbBbBhBhBoBoBoBuBuBuBuB{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B!�B!�B"�B"�B"�B#�B#�B$�B$�B$�B$�B$�B%�B%�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B+B+B,B,B-B-B-B-B.B.B.B/B0!B0!B0!B0!B1'B1'B1'B1'B2-B2-B2-B2-B2-B2-B2-B2-B33B33B33B49B49B49B5?B5?B6FB7LB7LB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB<jB<jB<jB<jB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BF�BF�BG�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211225100109                              AO  ARCAADJP                                                                    20211225100109    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211225100109  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211225100109  QCF$                G�O�G�O�G�O�8000            