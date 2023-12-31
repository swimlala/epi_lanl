CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:50:56Z creation;2022-06-04T17:50:57Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175056  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               $A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @����~K1   @���"���@0�?|�h�c,�hr�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�33B���B���B�  B�  B�  B�  B�  B���B�  B�  C   C�C�C�fC  C
  C�C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,L�C-�3C0  C2  C3�fC5�fC7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CLffCM��CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� DfD�fD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�C3DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @33@p  @�  @�  A  A=��A\  A|  A�  A�  A���A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� Bĳ3B��fB�L�B�L�Bӳ3B�L�B�L�B߀ B� B� B� B� B�L�B�� B�� B�� CٚCٚC�fC� C	� CٚC� C�fC� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C,�C-s3C/� C1� C3�fC5�fC7�fC9�fC;� C=� C?� CA� CC� CE� CG� CI� CL&fCMY�CO� CQ� CS� CU� CWٚCY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C��3C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D�fDvfD� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF�fDGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do�fDpvfDp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~�fDvfD� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�{3D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�{3D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D��3D�;3D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�k31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aɲ-Aɳ�Aɵ?Aɵ�AɶzAɷLAɷ�Aɸ�Aɹ$Aɺ^Aɺ�AɼjAɽAɼ6AɻdAɼ�A��UA���A���A��gA��9A�A��UA���A��ZA�Q�A�J#A�C�A�?�A�9$A�0�A�*0A�'A�"�A�=A�YA�bA�~A��A���A��A���A�ߤA�خA��NA���AɡbA�f�A�%�A���A���A��CA��wA�xlA�UgA�|PA�s�A�F�A��	A���A��DA���A�7LA�
�A��A���A��hA���A��!A��=A�}�A��VA�>A��>A��+A�_pA�0!A�`vA�� A�qAA�ݘA�B'A���A���A�^�A�@�A�*0A�� A��A�	lAs�Al��Af��Ac/�Aa�jA^1'AY��ASzxAN�oAJ=qAG�AC�DAAeA>�A<�mA;��A:�A:�A:=�A:�A9MA8�A86A7~�A7�A6��A5͟A5�A4��A3��A2��A3L0A2˒A2S&A2!-A2�A1��A0�\A.��A-�A,|A+�$A+.�A*�XA)��A(��A(�A(�A(MjA(�A'�`A(6zA'��A'&�A&t�A&E�A&L�A&�A&��A&k�A%M�A%�A$��A$a|A#�sA#S�A#�A"�	A"a�A!�+A >�A҉A��A
�A� A��Ay�AGAOAE9A$A$A��A�hAU�A��A*0A�LA!�AA�UA��AF�A��AMA�>A��A@�A�NA�A9XA��AخA��A/A�AѷA��AM�A��A�A.IA�A�pA��A\)A��A��A�)A!-A�PA��A��AN�A�A��AخA|AYA
ĜA
6A
�A	�`A	�gA	a|A��Ai�A�A�Ap�A	lA�BA�PA-A%A��AĜA�4Ag8AE9A	A^5AqA��A�HA�zAL�A  A��A{�A��A+�A ��A �A �UA �-A �VA h
A @�҉@�v�@��@�S�@�p;@���@��@�X�@�� @�X@��$@�M@�+�@��u@��@�s�@��m@�}�@�e�@�!-@�}@�l�@�~@��X@��@�h
@�{@��@��Q@���@�u@�3�@��@��X@耝@�A�@��@�=�@��M@��p@栐@�M@�%F@��@�/�@���@�r@� �@�!@�e@��@�"@�o@ߧ�@ޕ�@���@�H�@�-�@�U�@��@��c@��@�M�@�-�@ҹ$@��@�9X@�|�@��@�3�@�^�@��	@̜x@�Xy@��Q@��)@�A�@���@�{J@�P�@�8�@�!�@��)@ȣ�@Ȩ�@ȵ@���@Ȣ4@�PH@�!@��d@ǅ�@�k�@�!�@Ƙ_@��@�hs@�҉@�?�@�G@���@���@���@�A�@��@�7@���@���@��@�w2@�)_@�<6@��@�\�@�ԕ@�@@���@�d�@��t@�%@���@���@���@��@�V�@��j@��:@���@��L@�<�@�(�@�˒@��@�G�@�@@��'@�Q@��3@���@�x@��)@�p;@���@�c@�a@�J�@��1@�S�@�-�@��@���@�S�@���@�'R@�C@�~(@�e@��o@��[@���@�U�@�`B@��@�/�@���@�}�@�@@���@�1'@��]@��@��f@�a�@�RT@�%F@���@�Xy@�-�@��@���@�rG@�
=@���@�A�@�+k@��0@�n/@�zx@���@���@��@�@@�	l@���@�Ĝ@��@�9X@�7@���@��@��M@��O@���@�h
@�*�@���@� i@��h@�q@�<�@��w@�c�@�Y@��f@���@�=q@�J@��P@�+@�V@�(@���@�Ɇ@��@�r�@��W@�L�@��@��@��A@�YK@�G@��C@�O�@���@���@��r@�l"@�N�@��@��~@�S�@�!�@���@�tT@���@�@��x@�L0@��6@�$t@��f@���@�� @�}V@�_�@���@���@�Vm@�@@��4@�>B@�G@�ƨ@�\)@�4@�&�@�@��p@�� @�Xy@��@��w@���@�t�@�S@��.@�Z�@� �@��g@���@�(�@��<@�c @�+k@��T@��@�C�@�V@��|@���@���@���@�z�@�YK@�6@��@���@���@�0�@�֡@��R@�z@�^5@�B[@�7@���@���@��{@�n/@�a@�J#@�=@�@��j@�h�@�b@��@��r@��3@��-@���@�G�@���@��m@�c�@�~@��}@���@�x@�=@��@��9@���@�{�@�YK@�B[@�2�@�(�@�@v`@~�!@~J@}�t@}`B@}V@}�@|��@{�m@{33@z�,@z��@zs�@y�>@yx�@y@x��@x��@xK^@x@x�@w!-@v�x@u�o@u�@uVm@t�@t��@t�@sg�@s&@r�X@rd�@q��@qV@p�j@pg8@o�}@o{J@oZ�@o=@n��@m��@m2a@l�/@l�Y@lx@k�;@k��@j�@jTa@j	@i�o@i�@hA�@g�{@gx@ge�@f�@e�@d�@c�@cMj@b�8@b�,@b�2@b�@a@`��@`'R@_�K@^�8@^�h@]��@\I�@\�@[��@[Z�@[C�@['�@Z��@Z6�@Y�'@Ya�@Y8�@X�v@Xw�@XQ�@XFt@W|�@V�M@V�<@V��@Vv�@Vq�@Vc @V�@U�'@Uk�@UT�@U-w@T�_@TQ�@T  @S��@S@R�r@R$�@Q�@Q��@Q��@Qe,@P��@P�@O��@OO@N�@N�h@N�@M��@MJ�@L��@L��@Lg8@L?�@L@K�&@KF�@J^5@I�j@I��@If�@I:�@H�	@H��@H�e@HtT@HH@G��@G�@@Gb�@G�@F��@F�@F��@F� @F=q@Eo @E�@D�5@D�`@D�j@D]d@Dx@C��@C�*@C��@Cl�@C,�@CE9@C>�@B҉@B�@A�@A��@Azx@Ahs@A-w@@�	@@�@@�@@A�@@%�@@-�@@@?�@?"�@>��@>��@>Q@>.�@>&�@>�@=ԕ@=q@<�4@<[�@<M@;��@;~�@;g�@;.I@:��@:($@9�@9��@9�@9 \@82�@7�@7�a@7dZ@7/�@7�@6��@6�s@6l�@5@5��@5e,@4�[@4S�@3�g@3��@3J#@2��@2ff@1��@1G�@0֡@0��@0"h@/��@/o�@/&@.��@.�@.ں@.ȴ@.�6@.C�@-��@-�T@-��@-��@-T�@,Ĝ@,�o@,N�@,,=@,�@+�@+�@+�@+e�@+�@*�R@*0U@*!�@*	@)��@)��@)��@)�o@)k�@(��@(�?@(�j@(�$@(��@(`�@(,=@'��@'��@'��@'iD@'Y@&�,@&��@&�6@&v�@&kQ@&a|@&?@&O@& �@%�9@%��@%o @%�@$�$@$S�@$M@#�@#�6@#�@#v`@#9�@#�@"�@"��@"�L@"ff@";�@"�@!��@!�=@!�@!L�@!(�@!�@ ��@ �@ m�@ $@ �@ x@�6@Z�@��@��@L0@��@��@�M@��@�P@�[@�_@6@7@�@�K@��@��@��@'�@�@��@$�@@�@Vm@Dg@*0@;@�[@��@�.@q@I�@!@  @�K@��@�:@RT@1�@Y@�H@�F@;�@�Z@�@�h@[W@7L@%@�$@oi@$@�Q@��@�V@�P@Z�@'�@�@�s@�r@h
@-@�@s�@�@֡@��@~(@j@>B@�@��@˒@�f@O@9�@�@��@�h@��@s�@1�@�@��@�@}�@Y�@�@Ĝ@��@z�@bN@	�@��@�{@
�"@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aɲ-Aɳ�Aɵ?Aɵ�AɶzAɷLAɷ�Aɸ�Aɹ$Aɺ^Aɺ�AɼjAɽAɼ6AɻdAɼ�A��UA���A���A��gA��9A�A��UA���A��ZA�Q�A�J#A�C�A�?�A�9$A�0�A�*0A�'A�"�A�=A�YA�bA�~A��A���A��A���A�ߤA�خA��NA���AɡbA�f�A�%�A���A���A��CA��wA�xlA�UgA�|PA�s�A�F�A��	A���A��DA���A�7LA�
�A��A���A��hA���A��!A��=A�}�A��VA�>A��>A��+A�_pA�0!A�`vA�� A�qAA�ݘA�B'A���A���A�^�A�@�A�*0A�� A��A�	lAs�Al��Af��Ac/�Aa�jA^1'AY��ASzxAN�oAJ=qAG�AC�DAAeA>�A<�mA;��A:�A:�A:=�A:�A9MA8�A86A7~�A7�A6��A5͟A5�A4��A3��A2��A3L0A2˒A2S&A2!-A2�A1��A0�\A.��A-�A,|A+�$A+.�A*�XA)��A(��A(�A(�A(MjA(�A'�`A(6zA'��A'&�A&t�A&E�A&L�A&�A&��A&k�A%M�A%�A$��A$a|A#�sA#S�A#�A"�	A"a�A!�+A >�A҉A��A
�A� A��Ay�AGAOAE9A$A$A��A�hAU�A��A*0A�LA!�AA�UA��AF�A��AMA�>A��A@�A�NA�A9XA��AخA��A/A�AѷA��AM�A��A�A.IA�A�pA��A\)A��A��A�)A!-A�PA��A��AN�A�A��AخA|AYA
ĜA
6A
�A	�`A	�gA	a|A��Ai�A�A�Ap�A	lA�BA�PA-A%A��AĜA�4Ag8AE9A	A^5AqA��A�HA�zAL�A  A��A{�A��A+�A ��A �A �UA �-A �VA h
A @�҉@�v�@��@�S�@�p;@���@��@�X�@�� @�X@��$@�M@�+�@��u@��@�s�@��m@�}�@�e�@�!-@�}@�l�@�~@��X@��@�h
@�{@��@��Q@���@�u@�3�@��@��X@耝@�A�@��@�=�@��M@��p@栐@�M@�%F@��@�/�@���@�r@� �@�!@�e@��@�"@�o@ߧ�@ޕ�@���@�H�@�-�@�U�@��@��c@��@�M�@�-�@ҹ$@��@�9X@�|�@��@�3�@�^�@��	@̜x@�Xy@��Q@��)@�A�@���@�{J@�P�@�8�@�!�@��)@ȣ�@Ȩ�@ȵ@���@Ȣ4@�PH@�!@��d@ǅ�@�k�@�!�@Ƙ_@��@�hs@�҉@�?�@�G@���@���@���@�A�@��@�7@���@���@��@�w2@�)_@�<6@��@�\�@�ԕ@�@@���@�d�@��t@�%@���@���@���@��@�V�@��j@��:@���@��L@�<�@�(�@�˒@��@�G�@�@@��'@�Q@��3@���@�x@��)@�p;@���@�c@�a@�J�@��1@�S�@�-�@��@���@�S�@���@�'R@�C@�~(@�e@��o@��[@���@�U�@�`B@��@�/�@���@�}�@�@@���@�1'@��]@��@��f@�a�@�RT@�%F@���@�Xy@�-�@��@���@�rG@�
=@���@�A�@�+k@��0@�n/@�zx@���@���@��@�@@�	l@���@�Ĝ@��@�9X@�7@���@��@��M@��O@���@�h
@�*�@���@� i@��h@�q@�<�@��w@�c�@�Y@��f@���@�=q@�J@��P@�+@�V@�(@���@�Ɇ@��@�r�@��W@�L�@��@��@��A@�YK@�G@��C@�O�@���@���@��r@�l"@�N�@��@��~@�S�@�!�@���@�tT@���@�@��x@�L0@��6@�$t@��f@���@�� @�}V@�_�@���@���@�Vm@�@@��4@�>B@�G@�ƨ@�\)@�4@�&�@�@��p@�� @�Xy@��@��w@���@�t�@�S@��.@�Z�@� �@��g@���@�(�@��<@�c @�+k@��T@��@�C�@�V@��|@���@���@���@�z�@�YK@�6@��@���@���@�0�@�֡@��R@�z@�^5@�B[@�7@���@���@��{@�n/@�a@�J#@�=@�@��j@�h�@�b@��@��r@��3@��-@���@�G�@���@��m@�c�@�~@��}@���@�x@�=@��@��9@���@�{�@�YK@�B[@�2�@�(�@�@v`@~�!@~J@}�t@}`B@}V@}�@|��@{�m@{33@z�,@z��@zs�@y�>@yx�@y@x��@x��@xK^@x@x�@w!-@v�x@u�o@u�@uVm@t�@t��@t�@sg�@s&@r�X@rd�@q��@qV@p�j@pg8@o�}@o{J@oZ�@o=@n��@m��@m2a@l�/@l�Y@lx@k�;@k��@j�@jTa@j	@i�o@i�@hA�@g�{@gx@ge�@f�@e�@d�@c�@cMj@b�8@b�,@b�2@b�@a@`��@`'R@_�K@^�8@^�h@]��@\I�@\�@[��@[Z�@[C�@['�@Z��@Z6�@Y�'@Ya�@Y8�@X�v@Xw�@XQ�@XFt@W|�@V�M@V�<@V��@Vv�@Vq�@Vc @V�@U�'@Uk�@UT�@U-w@T�_@TQ�@T  @S��@S@R�r@R$�@Q�@Q��@Q��@Qe,@P��@P�@O��@OO@N�@N�h@N�@M��@MJ�@L��@L��@Lg8@L?�@L@K�&@KF�@J^5@I�j@I��@If�@I:�@H�	@H��@H�e@HtT@HH@G��@G�@@Gb�@G�@F��@F�@F��@F� @F=q@Eo @E�@D�5@D�`@D�j@D]d@Dx@C��@C�*@C��@Cl�@C,�@CE9@C>�@B҉@B�@A�@A��@Azx@Ahs@A-w@@�	@@�@@�@@A�@@%�@@-�@@@?�@?"�@>��@>��@>Q@>.�@>&�@>�@=ԕ@=q@<�4@<[�@<M@;��@;~�@;g�@;.I@:��@:($@9�@9��@9�@9 \@82�@7�@7�a@7dZ@7/�@7�@6��@6�s@6l�@5@5��@5e,@4�[@4S�@3�g@3��@3J#@2��@2ff@1��@1G�@0֡@0��@0"h@/��@/o�@/&@.��@.�@.ں@.ȴ@.�6@.C�@-��@-�T@-��@-��@-T�@,Ĝ@,�o@,N�@,,=@,�@+�@+�@+�@+e�@+�@*�R@*0U@*!�@*	@)��@)��@)��@)�o@)k�@(��@(�?@(�j@(�$@(��@(`�@(,=@'��@'��@'��@'iD@'Y@&�,@&��@&�6@&v�@&kQ@&a|@&?@&O@& �@%�9@%��@%o @%�@$�$@$S�@$M@#�@#�6@#�@#v`@#9�@#�@"�@"��@"�L@"ff@";�@"�@!��@!�=@!�@!L�@!(�@!�@ ��@ �@ m�@ $@ �@ x@�6@Z�@��@��@L0@��@��@�M@��@�P@�[@�_@6@7@�@�K@��@��@��@'�@�@��@$�@@�@Vm@Dg@*0@;@�[@��@�.@q@I�@!@  @�K@��@�:@RT@1�@Y@�H@�F@;�@�Z@�@�h@[W@7L@%@�$@oi@$@�Q@��@�V@�P@Z�@'�@�@�s@�r@h
@-@�@s�@�@֡@��@~(@j@>B@�@��@˒@�f@O@9�@�@��@�h@��@s�@1�@�@��@�@}�@Y�@�@Ĝ@��@z�@bN@	�@��@�{@
�"@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B��B�B��B��B�B�B�B�B�7B�7B�B��B�B�7B�QB�kB�QB�7B�QBڠB��B�jB	�B	��B	�HB	�NB	��B	��B	��B	��B	�NB	�hB	��B	�B	��B	�[B	��B	�oB	��B	��B	�B	��B	�2B	��B	��B
h>B
��B
�B
��B Ba-B|�B�fB��B�B�B�]B�B�(B��B��B��B� B��B �B�B��B��B�(BĜB�RB��B��B��B��B��BvzB^5BESB,�B�B
��B
�HB
��B
qB
U�B
�B	��B	��B	mCB	Q�B	FYB	1[B	�B	B�B��B��B̘BϫBЗBרB�B	'B		�B	HB	6FB	g�B	�iB	��B	�2B	�=B	��B	�&B	��B	�"B	��B	��B
gB
�B
%zB
&2B
%�B
%`B
#:B
�B
~B
�B

�B
DB
	�B
9B
 �B	��B
%B
�B
�B
;B
2|B
6�B
2�B
1�B
0�B
5tB
BB
E�B
D�B
A�B
@�B
?�B
@4B
HfB
NVB
OBB
SB
XB
X_B
TaB
I�B
B'B
?�B
?B
@�B
D�B
G�B
D3B
N�B
R�B
WYB
W�B
WYB
V9B
VB
T�B
S�B
QNB
RB
S&B
T{B
TFB
U2B
UMB
VB
U�B
T�B
S@B
R�B
S@B
R�B
RoB
RoB
RTB
Q�B
QhB
P�B
P}B
OB
L�B
J�B
KB
K)B
KB
J�B
JXB
IB
H�B
E9B
D�B
D�B
C�B
CGB
B�B
CaB
@B
?�B
>(B
>B
=B
<�B
;�B
;JB
;�B
:�B
:xB
:�B
:�B
9$B
8RB
8�B
:DB
9�B
;B
;�B
;�B
:^B
9�B
9�B
:B
9>B
88B
7�B
6�B
5�B
3�B
2|B
1�B
0�B
/�B
.�B
-�B
-�B
-�B
-]B
-)B
,B
*�B
(�B
'�B
'�B
'RB
&�B
%�B
$@B
"�B
�B
�B
IB
xB
�B
B
�B
gB
�B
�B
@B
�B
B
 B
�B
�B
<B
<B
B
.B
�B
�B
�B
B
bB
�B
�B
�B
�B
VB
�B
B
~B
xB
	�B
�B
EB
B
+B
�B
�B
�B
B
�B
fB
B
_B
YB
mB
�B
B	�B	�^B	�zB	�B	�=B	��B	�B	�B	�B	�:B	��B	�ZB	��B	��B	��B	��B	�B	�nB	��B	�:B	�nB	�B	��B	�B	�zB	�B	�>B	��B	��B	�B	�IB	�B	�'B	�B	��B	�B	�aB	�hB	��B	��B	�B	�B	�B	��B	�zB	�B	�LB	��B	��B	�B	�lB	��B	�0B	�JB	�^B	�dB	�<B	��B	��B	�B	��B	��B	��B	�wB	�wB	��B	�BB	��B	��B	�B	��B	��B	�cB	�.B	��B	�.B	��B	�wB	��B	��B	�.B	��B	��B	��B	�cB	�.B	��B	��B	�cB	�HB	��B	��B	��B	�PB	�B	�B	�B	�B	�*B	�*B	�^B	��B	�B	��B	��B	�B	�<B	�<B	�"B	��B	�VB	��B	��B	��B	��B	�B	�(B	�(B	��B	��B	��B	�B	��B
 �B
  B
�B
�B
B
�B
B
�B
�B
�B
?B
?B
?B
tB
%B
%B
�B
�B
�B
zB
�B
�B
�B
KB
�B
	7B
	�B
	�B

rB

�B
B

�B
DB
B
�B
B
�B
�B
�B
�B
B
<B
VB
�B
(B
�B
B
.B
}B
}B
bB
�B
}B
�B
 B
 B
 B
hB
B
B
:B
B
�B
�B
�B
�B
�B
�B
SB
mB
�B
�B
�B
�B
sB
�B
�B
+B
�B
eB
eB
1B
�B
B
�B
�B
�B
�B
�B
qB
qB
�B
�B
B
xB
�B
IB
dB
�B
OB
!B
pB
 B
 BB
 �B
!-B
!bB
!|B
"B
"hB
"�B
"�B
#B
#�B
#�B
$ZB
$�B
%�B
%�B
&2B
&�B
'mB
(
B
(�B
(�B
)�B
)�B
)�B
)�B
*eB
*KB
*�B
+6B
,=B
,�B
,�B
,�B
-wB
-�B
-�B
.IB
.�B
/5B
/OB
/OB
/�B
/�B
/�B
/�B
/�B
0�B
0�B
1[B
1[B
1[B
1[B
1'B
1AB
0�B
1B
0�B
0�B
1B
0�B
1AB
2GB
2�B
2aB
2�B
2|B
2�B
3B
3MB
3�B
4B
49B
4�B
4�B
4nB
5�B
5�B
6zB
6zB
6�B
6�B
7�B
8lB
8�B
9>B
9�B
:B
:�B
:�B
:�B
;B
;B
;�B
;�B
;dB
;�B
<�B
<jB
<6B
;�B
;B
:�B
:�B
;�B
;�B
<6B
<�B
<�B
<�B
;�B
;�B
;�B
;B
;dB
:�B
;B
<PB
<B
<B
<jB
<B
<�B
=VB
=qB
="B
=�B
=�B
>�B
?HB
?cB
?}B
?�B
?�B
?�B
@ B
@B
@�B
@�B
@�B
AoB
A�B
A�B
A�B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CB
C�B
C�B
C�B
C�B
DgB
DgB
D�B
D�B
D�B
D�B
D�B
D�B
D3B
C�B
C�B
C{B
C�B
C�B
C�B
DgB
EB
E9B
FYB
F�B
G�B
HfB
IB
I7B
IB
IB
I�B
J�B
J�B
KB
J�B
KB
K^B
KxB
K^B
K�B
KxB
K�B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
MB
N�B
O(B
O\B
OvB
P.B
O�B
O�B
P�B
QB
Q�B
QhB
QNB
Q�B
R B
R�B
R�B
SuB
S[B
SuB
S[B
S�B
S�B
S�B
S�B
TB
TB
S�B
S�B
TFB
T�B
T�B
UgB
U�B
U�B
U�B
U�B
U�B
V�B
V�B
W$B
WYB
W�B
W�B
W�B
XB
X_B
YB
YB
Y1B
Y1B
YeB
Z�B
Z�B
Z�B
[	B
[=B
[WB
[WB
[qB
[�B
\]B
\]B
\�B
]B
]~B
^B
^B
^OB
^jB
^�B
_B
_�B
_�B
`BB
`vB
`�B
`�B
aHB
a|B
abB
a|B
a|B
a�B
a�B
bB
bB
bB
bB
b4B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
c�B
dB
c�B
dB
dB
c�B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
d�B
eB
e,B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
hXB
h�B
h�B
iB
iB
iDB
i_B
i�B
i�B
i�B
i�B
jB
jB
jeB
jB
jB
j�B
kB
kB
kQB
kkB
kkB
k�B
lB
k�B
lqB
lWB
lWB
l�B
m]B
m�B
m�B
ncB
n}B
n�B
n�B
oOB
oB
o5B
oiB
o�B
o�B
pB
p;B
pUB
p;B
pUB
p�B
qB
q'B
qvB
q�B
q�B
rB
r-B
r-B
raB
raB
raB
r�B
r�B
r�B
r�B
r�B
shB
s�B
s�B
tB
tB
t9B
tTB
t�B
uB
u?B
utB
u�B
u�B
u�B
vB
vFB
v�B
v�B
wB
w2B
wfB
wLB
w�B
w�B
w�B
w�B
xRB
xRB
x�B
x�B
yXB
y�B
y�B
zB
zB
z*B
z^B
zxB
zxB
z�B
z�B
{0B
{B
{0B
{JB
{B
{�B
|B
|B
|6B
|6B
|PB
|PB
|6B
|�B
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B��B�B��B��B�B�B�B�B�7B�7B�B��B�B�7B�QB�kB�QB�7B�QBڠB��B�jB	�B	��B	�HB	�NB	��B	��B	��B	��B	�NB	�hB	��B	�B	��B	�[B	��B	�oB	��B	��B	�B	��B	�2B	��B	��B
h>B
��B
�B
��B Ba-B|�B�fB��B�B�B�]B�B�(B��B��B��B� B��B �B�B��B��B�(BĜB�RB��B��B��B��B��BvzB^5BESB,�B�B
��B
�HB
��B
qB
U�B
�B	��B	��B	mCB	Q�B	FYB	1[B	�B	B�B��B��B̘BϫBЗBרB�B	'B		�B	HB	6FB	g�B	�iB	��B	�2B	�=B	��B	�&B	��B	�"B	��B	��B
gB
�B
%zB
&2B
%�B
%`B
#:B
�B
~B
�B

�B
DB
	�B
9B
 �B	��B
%B
�B
�B
;B
2|B
6�B
2�B
1�B
0�B
5tB
BB
E�B
D�B
A�B
@�B
?�B
@4B
HfB
NVB
OBB
SB
XB
X_B
TaB
I�B
B'B
?�B
?B
@�B
D�B
G�B
D3B
N�B
R�B
WYB
W�B
WYB
V9B
VB
T�B
S�B
QNB
RB
S&B
T{B
TFB
U2B
UMB
VB
U�B
T�B
S@B
R�B
S@B
R�B
RoB
RoB
RTB
Q�B
QhB
P�B
P}B
OB
L�B
J�B
KB
K)B
KB
J�B
JXB
IB
H�B
E9B
D�B
D�B
C�B
CGB
B�B
CaB
@B
?�B
>(B
>B
=B
<�B
;�B
;JB
;�B
:�B
:xB
:�B
:�B
9$B
8RB
8�B
:DB
9�B
;B
;�B
;�B
:^B
9�B
9�B
:B
9>B
88B
7�B
6�B
5�B
3�B
2|B
1�B
0�B
/�B
.�B
-�B
-�B
-�B
-]B
-)B
,B
*�B
(�B
'�B
'�B
'RB
&�B
%�B
$@B
"�B
�B
�B
IB
xB
�B
B
�B
gB
�B
�B
@B
�B
B
 B
�B
�B
<B
<B
B
.B
�B
�B
�B
B
bB
�B
�B
�B
�B
VB
�B
B
~B
xB
	�B
�B
EB
B
+B
�B
�B
�B
B
�B
fB
B
_B
YB
mB
�B
B	�B	�^B	�zB	�B	�=B	��B	�B	�B	�B	�:B	��B	�ZB	��B	��B	��B	��B	�B	�nB	��B	�:B	�nB	�B	��B	�B	�zB	�B	�>B	��B	��B	�B	�IB	�B	�'B	�B	��B	�B	�aB	�hB	��B	��B	�B	�B	�B	��B	�zB	�B	�LB	��B	��B	�B	�lB	��B	�0B	�JB	�^B	�dB	�<B	��B	��B	�B	��B	��B	��B	�wB	�wB	��B	�BB	��B	��B	�B	��B	��B	�cB	�.B	��B	�.B	��B	�wB	��B	��B	�.B	��B	��B	��B	�cB	�.B	��B	��B	�cB	�HB	��B	��B	��B	�PB	�B	�B	�B	�B	�*B	�*B	�^B	��B	�B	��B	��B	�B	�<B	�<B	�"B	��B	�VB	��B	��B	��B	��B	�B	�(B	�(B	��B	��B	��B	�B	��B
 �B
  B
�B
�B
B
�B
B
�B
�B
�B
?B
?B
?B
tB
%B
%B
�B
�B
�B
zB
�B
�B
�B
KB
�B
	7B
	�B
	�B

rB

�B
B

�B
DB
B
�B
B
�B
�B
�B
�B
B
<B
VB
�B
(B
�B
B
.B
}B
}B
bB
�B
}B
�B
 B
 B
 B
hB
B
B
:B
B
�B
�B
�B
�B
�B
�B
SB
mB
�B
�B
�B
�B
sB
�B
�B
+B
�B
eB
eB
1B
�B
B
�B
�B
�B
�B
�B
qB
qB
�B
�B
B
xB
�B
IB
dB
�B
OB
!B
pB
 B
 BB
 �B
!-B
!bB
!|B
"B
"hB
"�B
"�B
#B
#�B
#�B
$ZB
$�B
%�B
%�B
&2B
&�B
'mB
(
B
(�B
(�B
)�B
)�B
)�B
)�B
*eB
*KB
*�B
+6B
,=B
,�B
,�B
,�B
-wB
-�B
-�B
.IB
.�B
/5B
/OB
/OB
/�B
/�B
/�B
/�B
/�B
0�B
0�B
1[B
1[B
1[B
1[B
1'B
1AB
0�B
1B
0�B
0�B
1B
0�B
1AB
2GB
2�B
2aB
2�B
2|B
2�B
3B
3MB
3�B
4B
49B
4�B
4�B
4nB
5�B
5�B
6zB
6zB
6�B
6�B
7�B
8lB
8�B
9>B
9�B
:B
:�B
:�B
:�B
;B
;B
;�B
;�B
;dB
;�B
<�B
<jB
<6B
;�B
;B
:�B
:�B
;�B
;�B
<6B
<�B
<�B
<�B
;�B
;�B
;�B
;B
;dB
:�B
;B
<PB
<B
<B
<jB
<B
<�B
=VB
=qB
="B
=�B
=�B
>�B
?HB
?cB
?}B
?�B
?�B
?�B
@ B
@B
@�B
@�B
@�B
AoB
A�B
A�B
A�B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CB
C�B
C�B
C�B
C�B
DgB
DgB
D�B
D�B
D�B
D�B
D�B
D�B
D3B
C�B
C�B
C{B
C�B
C�B
C�B
DgB
EB
E9B
FYB
F�B
G�B
HfB
IB
I7B
IB
IB
I�B
J�B
J�B
KB
J�B
KB
K^B
KxB
K^B
K�B
KxB
K�B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
MB
N�B
O(B
O\B
OvB
P.B
O�B
O�B
P�B
QB
Q�B
QhB
QNB
Q�B
R B
R�B
R�B
SuB
S[B
SuB
S[B
S�B
S�B
S�B
S�B
TB
TB
S�B
S�B
TFB
T�B
T�B
UgB
U�B
U�B
U�B
U�B
U�B
V�B
V�B
W$B
WYB
W�B
W�B
W�B
XB
X_B
YB
YB
Y1B
Y1B
YeB
Z�B
Z�B
Z�B
[	B
[=B
[WB
[WB
[qB
[�B
\]B
\]B
\�B
]B
]~B
^B
^B
^OB
^jB
^�B
_B
_�B
_�B
`BB
`vB
`�B
`�B
aHB
a|B
abB
a|B
a|B
a�B
a�B
bB
bB
bB
bB
b4B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
c�B
dB
c�B
dB
dB
c�B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
d�B
eB
e,B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
hXB
h�B
h�B
iB
iB
iDB
i_B
i�B
i�B
i�B
i�B
jB
jB
jeB
jB
jB
j�B
kB
kB
kQB
kkB
kkB
k�B
lB
k�B
lqB
lWB
lWB
l�B
m]B
m�B
m�B
ncB
n}B
n�B
n�B
oOB
oB
o5B
oiB
o�B
o�B
pB
p;B
pUB
p;B
pUB
p�B
qB
q'B
qvB
q�B
q�B
rB
r-B
r-B
raB
raB
raB
r�B
r�B
r�B
r�B
r�B
shB
s�B
s�B
tB
tB
t9B
tTB
t�B
uB
u?B
utB
u�B
u�B
u�B
vB
vFB
v�B
v�B
wB
w2B
wfB
wLB
w�B
w�B
w�B
w�B
xRB
xRB
x�B
x�B
yXB
y�B
y�B
zB
zB
z*B
z^B
zxB
zxB
z�B
z�B
{0B
{B
{0B
{JB
{B
{�B
|B
|B
|6B
|6B
|PB
|PB
|6B
|�B
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104949  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175056  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175057  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175057                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025104  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025104  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                