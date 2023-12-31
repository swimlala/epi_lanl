CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-11T00:35:26Z creation;2018-01-11T00:35:30Z conversion to V3.1;2019-12-19T07:47:58Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180111003526  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_199                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�C����1   @�C�{B_ @41�����dm��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� DfD� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @|��@�  @�  A  A<  A\  A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc�fCe� Cg� CiٚCk� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D�fDp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~�fDp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�;3D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�{3D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D��3D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�8 D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�{3D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʛ�Aʛ�Aʟ�Aʟ�Aʟ�Aʟ�Aʡ�Aʡ�Aʡ�Aʡ�Aʣ�Aʥ�Aʧ�Aʩ�Aʩ�AʬAʩ�Aʩ�AʮAʰ!Aʲ-Aʲ-AʶFAʸRAʸRAʼjAʼjAʴ9Aʕ�A�?}A��A�oA�bA�JA��A�33A�$�A�1A���A��A��HA���Aə�A�ZA�S�A�G�A�7LA�/AȰ!AǶFA�9XA�&�A��AƾwAƏ\A�9XA���AŅA�K�A�=qA�"�AĴ9A��;A��A��A��A��A��FA��#A�O�A�x�A���A�x�A��A�z�A��-A�A��+A��mA���A�K�A��A�`BA���A���A��mA�VA�~�A��TA�7LA�"�A��9A��;A���A�7LA��TA��uA��FA���A��9A���A���A�\)A���A�^5A�n�A��A�ƨA���A�$�A���A�~�A�VA�n�A�(�A�l�A�A�A~�A}�A|ȴA|(�Ay��AxAu��AtQ�As��As�Aq7LAo��Am�^Al1'AkAj��Ah~�Af��Ac�Ab�9A_�^A]AZ�AY/AX�AW\)AV�jAU�mAS|�ARZAQ��AQAPv�AP{AN��AM��AK�AJ5?AG��AE�AD�HAC;dAB$�AA`BA@�yA?K�A>��A>E�A=�A=G�A<��A<bA;dZA:^5A8�!A6�A5"�A3�
A3VA2$�A/�
A-�A-C�A,��A*��A(=qA'�A%ƨA%?}A#O�A!�A �/AoA�jAbA��A�PA�^An�A��A�9AG�A�A�;A`BA%A�A�AQ�Ap�A1AVA
��A	�7AE�A�+A��A�jAn�A(�AdZA ��A �+@�;d@�J@�C�@���@�?}@�dZ@���@�j@��;@�l�@�@�`B@���@�+@���@�w@�C�@���@���@���@�!@�\@�`B@㝲@��T@�33@އ+@���@ܴ9@���@��H@�?}@�I�@�t�@�=q@��@�bN@�+@�ȴ@��@Гu@�C�@��@�M�@Ͳ-@�V@̛�@�+@���@��;@�K�@��H@ƸR@�{@���@Ĭ@�ƨ@§�@�-@��-@�j@���@��F@�+@��@���@�{@�/@� �@��@��H@��@��@�7L@��D@��F@��@�@�E�@��-@���@�bN@�ƨ@�l�@�\)@�S�@���@��#@��7@�x�@�&�@�Ĝ@�j@��F@�dZ@�"�@��@���@�$�@���@�G�@��@���@��@�Q�@���@���@��P@��y@�~�@�ff@�ff@�-@�{@���@��^@�&�@��j@�1'@�1@��P@�;d@��y@�5?@��T@�7L@���@�z�@�9X@�z�@���@�Z@�9X@��
@��@��@�9X@�(�@�  @���@��@���@���@�V@��#@�x�@�/@��@��F@��P@��P@�\)@���@���@�~�@�-@�5?@���@�@�@���@���@���@��@�Q�@�9X@�9X@�1@�(�@�b@���@��P@�S�@��@�@��H@��!@��R@���@�n�@�-@���@�/@���@�z�@�Q�@�I�@�A�@�(�@��
@��@�;d@��H@���@�n�@�=q@�@��@��T@��#@��#@���@�p�@�7L@��/@���@��@��u@�j@�A�@���@��@�K�@�"�@�
=@��@��h@�%@��9@��D@�I�@�A�@���@���@�\)@�|�@�l�@�"�@�ȴ@���@���@��!@���@�-@�@��@�@��7@�/@�bN@� �@�(�@� �@�b@���@��@�b@��
@��;@��@�;d@���@��!@���@�~�@�^5@�5?@�J@��T@�@�p�@�O�@��@��`@��@��9@��@�bN@�1@��@��@��@�dZ@�33@��R@�v�@�V@�=q@�$�@��@���@�p�@�/@�%@��@�Ĝ@���@�I�@��m@���@���@���@��@�|�@�C�@��@��@��R@�^5@�-@�{@��T@��-@��h@��7@�x�@�/@�V@��9@��D@�Q�@�(�@�@K�@;d@+@
=@~��@~5?@~@}��@}�h@}?}@}V@|�/@|j@{��@{"�@z�H@z�!@z^5@z-@zJ@yhs@x��@x��@xĜ@xA�@xb@xb@w�;@w\)@v�@v��@u�@up�@t�/@tz�@t(�@s�@s33@r��@rM�@q��@qhs@qG�@q&�@p��@pQ�@p  @o;d@n��@n��@nv�@nV@n$�@m�T@m��@mV@l9X@k��@k@j��@jn�@jJ@i�#@iX@h��@hbN@g��@g
=@g
=@f��@e�T@e`B@dz�@c�
@ct�@c@b��@b��@b-@a��@a�^@a��@a7L@`�9@_��@_�@^�@^��@^E�@]�@]��@]�@\�@\�D@\�@[S�@Z�H@Z~�@Z^5@Y�@Y��@Y��@Y�7@Yhs@X��@X�@X1'@W�w@W�P@Wl�@W;d@Vv�@U@Up�@T��@T�D@TI�@S�m@S�@SS�@S33@R��@R-@Q�#@Q�^@Q��@QX@Q%@P��@PQ�@P1'@P  @OK�@N�y@N��@Nff@N5?@M��@M�h@MO�@MO�@M�@L��@L�/@L�@Lj@Kƨ@K��@K33@J��@Jn�@J=q@J-@I�#@IX@I%@H�`@H�u@HQ�@H1'@Hb@G�;@G�w@G��@Gl�@G\)@GK�@F�@F�+@FE�@E�T@E��@Ep�@E`B@EO�@E�@EV@D�@Dz�@D(�@C�
@C�@C33@B��@B~�@B-@BJ@A�@A�#@AX@@�9@@1'@@b@?�@?l�@?\)@?K�@?+@?�@?
=@>��@>V@>@=�h@=`B@=/@=V@<�/@<�@<Z@;��@;C�@;o@:�@:^5@:=q@9��@9��@9hs@9G�@9%@8�`@8��@8Q�@8 �@7�;@7�P@7\)@7+@7
=@7
=@6��@6ȴ@6��@6��@6v�@6E�@5�@5�T@5��@4�@4��@4j@4�@3ƨ@3��@3��@3�@3t�@3S�@2�H@2�!@2��@2�\@2�\@2~�@2^5@1��@1x�@1X@1G�@1%@0�9@0bN@0 �@/�@/��@/�P@/
=@.��@.$�@-�-@-O�@,��@,�/@,�/@,�/@,�j@,Z@+��@+33@+"�@*�@*�!@*M�@)��@)�7@)X@)&�@(��@(��@(bN@'�;@'��@'|�@'K�@&ȴ@&E�@&{@%��@%�h@%?}@$��@$��@$��@$I�@$1@#��@#�m@#�m@#�
@#ƨ@#��@#"�@"��@"�\@"~�@"n�@"^5@"M�@"^5@"M�@!��@!�^@!�^@!��@!�7@!x�@!G�@!7L@!7L@!&�@!%@ ��@ ��@ �u@ �@ r�@ A�@ b@�@��@�@�P@l�@\)@;d@ȴ@�+@V@E�@�T@?}@?}@/@/@V@��@z�@j@I�@9X@�@ƨ@t�@C�@"�@@�@��@��@M�@-@�#@��@��@�7@hs@X@G�@G�@G�@&�@%@��@��@�u@�@bN@bN@bN@A�@�;@�w@��@l�@+@ȴ@��@��@v�@ff@ff@E�@5?@$�@$�@$�@$�@{@{@{@�T@�h@`B@�@�@�@z�@Z@I�@9X@1@�
@�F@dZ@33@"�@o@�@�!@��@�\@�\@^5@J@�#@�^@��@�7@�7@x�@hs@hs@7L@%@��@bN@ �@  @�@�;@��@�@�@�P@|�@l�@l�@�@�@�R@��@v�@{@�T@@��@p�@`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʛ�Aʛ�Aʟ�Aʟ�Aʟ�Aʟ�Aʡ�Aʡ�Aʡ�Aʡ�Aʣ�Aʥ�Aʧ�Aʩ�Aʩ�AʬAʩ�Aʩ�AʮAʰ!Aʲ-Aʲ-AʶFAʸRAʸRAʼjAʼjAʴ9Aʕ�A�?}A��A�oA�bA�JA��A�33A�$�A�1A���A��A��HA���Aə�A�ZA�S�A�G�A�7LA�/AȰ!AǶFA�9XA�&�A��AƾwAƏ\A�9XA���AŅA�K�A�=qA�"�AĴ9A��;A��A��A��A��A��FA��#A�O�A�x�A���A�x�A��A�z�A��-A�A��+A��mA���A�K�A��A�`BA���A���A��mA�VA�~�A��TA�7LA�"�A��9A��;A���A�7LA��TA��uA��FA���A��9A���A���A�\)A���A�^5A�n�A��A�ƨA���A�$�A���A�~�A�VA�n�A�(�A�l�A�A�A~�A}�A|ȴA|(�Ay��AxAu��AtQ�As��As�Aq7LAo��Am�^Al1'AkAj��Ah~�Af��Ac�Ab�9A_�^A]AZ�AY/AX�AW\)AV�jAU�mAS|�ARZAQ��AQAPv�AP{AN��AM��AK�AJ5?AG��AE�AD�HAC;dAB$�AA`BA@�yA?K�A>��A>E�A=�A=G�A<��A<bA;dZA:^5A8�!A6�A5"�A3�
A3VA2$�A/�
A-�A-C�A,��A*��A(=qA'�A%ƨA%?}A#O�A!�A �/AoA�jAbA��A�PA�^An�A��A�9AG�A�A�;A`BA%A�A�AQ�Ap�A1AVA
��A	�7AE�A�+A��A�jAn�A(�AdZA ��A �+@�;d@�J@�C�@���@�?}@�dZ@���@�j@��;@�l�@�@�`B@���@�+@���@�w@�C�@���@���@���@�!@�\@�`B@㝲@��T@�33@އ+@���@ܴ9@���@��H@�?}@�I�@�t�@�=q@��@�bN@�+@�ȴ@��@Гu@�C�@��@�M�@Ͳ-@�V@̛�@�+@���@��;@�K�@��H@ƸR@�{@���@Ĭ@�ƨ@§�@�-@��-@�j@���@��F@�+@��@���@�{@�/@� �@��@��H@��@��@�7L@��D@��F@��@�@�E�@��-@���@�bN@�ƨ@�l�@�\)@�S�@���@��#@��7@�x�@�&�@�Ĝ@�j@��F@�dZ@�"�@��@���@�$�@���@�G�@��@���@��@�Q�@���@���@��P@��y@�~�@�ff@�ff@�-@�{@���@��^@�&�@��j@�1'@�1@��P@�;d@��y@�5?@��T@�7L@���@�z�@�9X@�z�@���@�Z@�9X@��
@��@��@�9X@�(�@�  @���@��@���@���@�V@��#@�x�@�/@��@��F@��P@��P@�\)@���@���@�~�@�-@�5?@���@�@�@���@���@���@��@�Q�@�9X@�9X@�1@�(�@�b@���@��P@�S�@��@�@��H@��!@��R@���@�n�@�-@���@�/@���@�z�@�Q�@�I�@�A�@�(�@��
@��@�;d@��H@���@�n�@�=q@�@��@��T@��#@��#@���@�p�@�7L@��/@���@��@��u@�j@�A�@���@��@�K�@�"�@�
=@��@��h@�%@��9@��D@�I�@�A�@���@���@�\)@�|�@�l�@�"�@�ȴ@���@���@��!@���@�-@�@��@�@��7@�/@�bN@� �@�(�@� �@�b@���@��@�b@��
@��;@��@�;d@���@��!@���@�~�@�^5@�5?@�J@��T@�@�p�@�O�@��@��`@��@��9@��@�bN@�1@��@��@��@�dZ@�33@��R@�v�@�V@�=q@�$�@��@���@�p�@�/@�%@��@�Ĝ@���@�I�@��m@���@���@���@��@�|�@�C�@��@��@��R@�^5@�-@�{@��T@��-@��h@��7@�x�@�/@�V@��9@��D@�Q�@�(�@�@K�@;d@+@
=@~��@~5?@~@}��@}�h@}?}@}V@|�/@|j@{��@{"�@z�H@z�!@z^5@z-@zJ@yhs@x��@x��@xĜ@xA�@xb@xb@w�;@w\)@v�@v��@u�@up�@t�/@tz�@t(�@s�@s33@r��@rM�@q��@qhs@qG�@q&�@p��@pQ�@p  @o;d@n��@n��@nv�@nV@n$�@m�T@m��@mV@l9X@k��@k@j��@jn�@jJ@i�#@iX@h��@hbN@g��@g
=@g
=@f��@e�T@e`B@dz�@c�
@ct�@c@b��@b��@b-@a��@a�^@a��@a7L@`�9@_��@_�@^�@^��@^E�@]�@]��@]�@\�@\�D@\�@[S�@Z�H@Z~�@Z^5@Y�@Y��@Y��@Y�7@Yhs@X��@X�@X1'@W�w@W�P@Wl�@W;d@Vv�@U@Up�@T��@T�D@TI�@S�m@S�@SS�@S33@R��@R-@Q�#@Q�^@Q��@QX@Q%@P��@PQ�@P1'@P  @OK�@N�y@N��@Nff@N5?@M��@M�h@MO�@MO�@M�@L��@L�/@L�@Lj@Kƨ@K��@K33@J��@Jn�@J=q@J-@I�#@IX@I%@H�`@H�u@HQ�@H1'@Hb@G�;@G�w@G��@Gl�@G\)@GK�@F�@F�+@FE�@E�T@E��@Ep�@E`B@EO�@E�@EV@D�@Dz�@D(�@C�
@C�@C33@B��@B~�@B-@BJ@A�@A�#@AX@@�9@@1'@@b@?�@?l�@?\)@?K�@?+@?�@?
=@>��@>V@>@=�h@=`B@=/@=V@<�/@<�@<Z@;��@;C�@;o@:�@:^5@:=q@9��@9��@9hs@9G�@9%@8�`@8��@8Q�@8 �@7�;@7�P@7\)@7+@7
=@7
=@6��@6ȴ@6��@6��@6v�@6E�@5�@5�T@5��@4�@4��@4j@4�@3ƨ@3��@3��@3�@3t�@3S�@2�H@2�!@2��@2�\@2�\@2~�@2^5@1��@1x�@1X@1G�@1%@0�9@0bN@0 �@/�@/��@/�P@/
=@.��@.$�@-�-@-O�@,��@,�/@,�/@,�/@,�j@,Z@+��@+33@+"�@*�@*�!@*M�@)��@)�7@)X@)&�@(��@(��@(bN@'�;@'��@'|�@'K�@&ȴ@&E�@&{@%��@%�h@%?}@$��@$��@$��@$I�@$1@#��@#�m@#�m@#�
@#ƨ@#��@#"�@"��@"�\@"~�@"n�@"^5@"M�@"^5@"M�@!��@!�^@!�^@!��@!�7@!x�@!G�@!7L@!7L@!&�@!%@ ��@ ��@ �u@ �@ r�@ A�@ b@�@��@�@�P@l�@\)@;d@ȴ@�+@V@E�@�T@?}@?}@/@/@V@��@z�@j@I�@9X@�@ƨ@t�@C�@"�@@�@��@��@M�@-@�#@��@��@�7@hs@X@G�@G�@G�@&�@%@��@��@�u@�@bN@bN@bN@A�@�;@�w@��@l�@+@ȴ@��@��@v�@ff@ff@E�@5?@$�@$�@$�@$�@{@{@{@�T@�h@`B@�@�@�@z�@Z@I�@9X@1@�
@�F@dZ@33@"�@o@�@�!@��@�\@�\@^5@J@�#@�^@��@�7@�7@x�@hs@hs@7L@%@��@bN@ �@  @�@�;@��@�@�@�P@|�@l�@l�@�@�@�R@��@v�@{@�T@@��@p�@`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BoBuBuBuBuBuBuBuBuBuB{B{B{B�B�B�B{B{B�B�B�B�B�B�B�B$�B%�B+B-B.B/B0!B1'B5?BI�Bs�B�3B�dB�^B�XB�LB�?B�LB��B�B�B�B�B�)B��BPBJBuB'�B1'B?}BB�BG�BS�BS�BI�B<jB�B��B�B��BB�fB�ZB��B+BPB1B  B�B
=B	7B��B��B�B�B��B��B�`B�B��B�)B��B�dB�XB�dB�!B�JB�DB��B��B}�B\)BYBM�B#�B
�BB
�B
�RB
��B
��B
�B
n�B
s�B
G�B
p�B
^5B
XB
F�B
�B
&�B
�B
#�B
�B	��B	��B	�B	�B	�B	�yB	�B	�B	ɺB	ÖB	ǮB	�XB	��B	��B	~�B	~�B	ffB	]/B	L�B	N�B	M�B	M�B	G�B	<jB	+B	(�B	.B	,B	'�B	"�B	�B	
=B��B�B�`B�B�B�5B�BB�/B�5B��B�B�B��B�
B��B��BɺBĜB�RB��B��B��B��B��B�JB�+B��B�hB�Bq�B�B�B�Br�Br�Bp�Be`Bs�Bq�Bn�Bl�B\)BF�BVBW
BL�BF�BS�B_;B_;BT�BS�BYBR�BO�BT�B[#BP�BF�B>wB2-BJ�BZBW
BQ�BN�BQ�BG�BI�BC�BI�BQ�BJ�BE�BW
BW
BR�BJ�B7LBM�BN�BS�BW
B`BBbNBiyBiyBjBgmB`BBXBT�BO�B_;BbNBbNBgmBdZBaHBffBgmBdZBgmBk�BjBq�Bn�Bn�Bq�B{�Bz�B{�B{�B{�Bt�Br�B�B�7B�JB�VB�DB�7B�oB�VB�bB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�^B�XB�dBĜBÖBÖBɺB��B��B�B�)B�;B�;B�)B�BB�sB�B�B�B�B�B��B��B	  B	B	B	1B	JB	hB	�B	�B	�B	�B	�B	�B	�B	$�B	+B	-B	.B	1'B	0!B	2-B	0!B	49B	49B	8RB	7LB	:^B	;dB	;dB	A�B	A�B	G�B	I�B	O�B	T�B	ZB	YB	^5B	]/B	`BB	hsB	iyB	iyB	iyB	k�B	jB	q�B	u�B	s�B	v�B	t�B	w�B	t�B	{�B	�B	�B	�%B	�%B	�JB	�PB	�\B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�?B	�9B	�9B	�LB	�RB	�^B	�dB	�jB	�jB	�}B	��B	��B	��B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�NB	�TB	�TB	�HB	�#B	�)B	�;B	�HB	�HB	�TB	�TB	�NB	�ZB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
  B	��B
  B
B
  B	��B
B
B
B
B
B
B
B
%B
+B
1B
+B
1B
+B
1B
DB
VB
VB
PB
PB
JB
PB
PB
PB
PB
\B
bB
\B
bB
hB
hB
hB
\B
bB
\B
hB
hB
hB
hB
oB
{B
{B
uB
oB
uB
{B
{B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
"�B
"�B
"�B
!�B
 �B
"�B
#�B
%�B
%�B
$�B
%�B
$�B
$�B
&�B
%�B
%�B
(�B
'�B
%�B
'�B
&�B
(�B
+B
,B
-B
-B
,B
-B
-B
-B
,B
,B
+B
-B
0!B
0!B
/B
/B
0!B
/B
0!B
1'B
0!B
/B
2-B
2-B
49B
33B
49B
5?B
49B
49B
33B
49B
5?B
49B
6FB
5?B
5?B
33B
49B
5?B
6FB
6FB
8RB
7LB
8RB
9XB
9XB
8RB
8RB
:^B
;dB
;dB
:^B
:^B
:^B
;dB
;dB
;dB
:^B
;dB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
>wB
=qB
?}B
?}B
?}B
A�B
A�B
A�B
@�B
@�B
A�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
H�B
G�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
K�B
J�B
I�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
K�B
J�B
L�B
M�B
M�B
L�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
Q�B
R�B
Q�B
P�B
R�B
S�B
S�B
S�B
T�B
VB
VB
VB
T�B
S�B
VB
W
B
W
B
W
B
VB
VB
S�B
VB
W
B
W
B
W
B
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
VB
VB
W
B
W
B
XB
YB
ZB
ZB
ZB
YB
XB
W
B
YB
ZB
ZB
YB
YB
YB
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
]/B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
`BB
aHB
aHB
cTB
bNB
cTB
bNB
bNB
bNB
aHB
bNB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
e`B
e`B
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
e`B
dZB
ffB
ffB
ffB
e`B
e`B
hsB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
jB
jB
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
l�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
k�B
l�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
l�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�BuB�BuBuB�BuB�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B&2B+kB-�B.}B/5B0;B1'B5tBI�BtB��B�B��B��B��B��B�B�B�KB�eBچB�sB��B��B�BBB(�B2B@OBCGBH1BTFBT�BKDB>�B�B�B��B��B��B�sB�B��B�BVB	lB�B�[B�BB��B��B��B�|B�6B��B�XBیB�BݘB��B��B��B��B��B� B�B��B��B��B`�B[�BP}B(sB
�lB�B
��B
�<B
�B
�B
�zB
sB
v�B
MB
q�B
`BB
YKB
IB
5B
(�B
�B
$�B
�B	��B	�<B	�UB	�;B	�B	��B	ؓB	�B	�B	�SB	ȚB	�0B	��B	�B	��B	�B	j0B	_�B	PB	Q B	OBB	N�B	H�B	>B	-�B	*B	/B	,�B	(�B	#�B	?B	JB�VB�-B�XB��B��B�\B�B�jB�;B��B��B�B�BרB��B��B��B�tB��B�B�B��B�_B��B�\B��B��B��B��Bu%B��B��B�YBuZBt�BraBg�BtnBr�Bo�Bm�B_!BJ�BWsBX�BO(BI�BU�B`'B`'BV�BU�BZ7BT{BQ�BVmB\)BR�BH�BA;B5�BL0BZ�BW�BS&BO�BR�BIBJ�BE�BKBR�BLdBGzBWsBW�BS�BLJB:xBN�BPBUBW�B`�Bb�Bi�Bi�Bj�Bh
BabBYBVmBQ�B_�BcBc:Bh$Be`Bb�BgRBhXBezBhXBlqBk�Br-Bo�Bo�Br�B|PB{�B|�B|�B|�BvBt�B��B��B��B��B��B�#B��B�BB�hB�+B�1B��B�'B�:B�NB�`B�FB�tB��B��B�}B��B��B��B��B�B�6B��B�3B�MB�XB̈́BңBևB�xB�pBߊB��B��B��B��B�B�B�B�GB�DB�]B	 iB	uB	�B	�B	�B	�B	�B	�B	�B		B	B	!B	;B	%FB	+QB	-CB	.}B	1[B	0oB	2|B	0�B	4�B	4�B	8�B	7�B	:�B	;�B	<B	A�B	BAB	HB	J=B	PB	T�B	ZQB	YB	^jB	]�B	`vB	hXB	i�B	i�B	i�B	k�B	k6B	q�B	u�B	t9B	wLB	uZB	xRB	u�B	|PB	�UB	�mB	�tB	��B	�~B	��B	��B	��B	��B	��B	��B	�$B	��B	�B	�B	�8B	�6B	�IB	�OB	�ZB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	��B	��B	�"B	�B	�PB	�6B	�"B	�6B	�(B	�BB	�HB	�:B	�FB	�2B	�2B	�MB	�MB	�YB	�_B	�B	�IB	�dB	ބB	ބB	ޞB	޸B	ޞB	�B	�B	�B	��B	�B	��B	ߊB	�|B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�"B	��B	��B	�B	��B	��B	��B	�B	�2B	�*B	�8B	�>B	�>B	�0B	�0B	�0B	�B	�B	�0B	�0B	�6B	�0B	�"B	�]B	�HB	�HB
 B
 OB	�]B	�]B
 4B	�HB
 4B
;B
 iB	�]B
UB
GB
GB
GB
[B
aB
mB
tB
_B
fB
zB
�B
�B
�B
�B
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
	B
�B
�B
B
�B
B
B
B
!B
�B
!B
!B
 B
!B
 B
 B
"B
"B
$B
$B
#B
#B
#B
"4B
!HB
# B
$@B
&2B
&B
%,B
&2B
%,B
%FB
'8B
&LB
&LB
)B
(>B
&fB
(>B
'mB
)DB
+QB
,WB
-CB
-CB
,WB
-CB
-CB
-CB
,qB
,WB
+�B
-wB
0UB
0UB
/OB
/�B
0UB
/iB
0�B
1[B
0oB
/�B
2|B
2|B
4�B
3�B
4nB
5tB
4nB
4�B
3�B
4�B
5tB
4�B
6zB
5tB
5tB
3�B
4�B
5�B
6�B
6�B
8�B
7�B
8�B
9�B
9�B
8�B
8�B
:�B
;�B
;�B
:�B
:�B
:�B
;�B
;�B
;�B
:�B
;�B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?�B
>�B
>�B
=�B
?�B
?�B
?�B
A�B
A�B
A�B
@�B
@�B
A�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
C�B
C�B
D�B
EB
E�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
IB
G�B
H�B
H�B
H�B
G�B
GB
IB
I�B
I�B
I�B
J�B
K�B
KB
K�B
J�B
J	B
J�B
KB
KB
K�B
MB
MB
MB
MB
LB
K)B
MB
NB
NB
MB
OB
O(B
N<B
O(B
PB
P.B
PB
OB
PB
PB
Q4B
QB
R:B
R:B
R:B
S&B
SB
R B
S&B
S&B
S@B
S&B
R B
S&B
R B
Q4B
S&B
T,B
T,B
TFB
U2B
VB
VB
VB
UMB
TFB
VSB
W$B
W$B
W$B
V9B
V9B
TaB
VSB
W?B
W?B
W?B
V9B
W?B
WYB
W?B
W?B
WYB
VmB
VmB
WYB
WYB
XEB
YKB
Z7B
ZQB
Z7B
YKB
X_B
W�B
YeB
ZQB
ZQB
YeB
YeB
YB
\]B
\]B
\]B
\]B
\]B
\]B
\�B
]dB
^�B
]dB
\�B
]~B
^�B
^�B
^jB
_pB
_pB
_pB
_pB
_pB
_�B
`\B
`vB
abB
abB
a|B
a|B
`�B
a|B
a|B
c�B
b�B
cnB
b�B
bhB
b�B
a|B
b�B
d�B
dtB
d�B
dtB
d�B
dtB
ezB
ezB
d�B
ezB
e�B
d�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
e�B
d�B
f�B
f�B
f�B
e�B
e�B
h�B
h�B
h�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
g�B
h�B
i�B
i�B
j�B
j�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
k�B
l�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
l�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
tB
s�B
t�B
t�B
uB
t�B
u�B
u�B
v�B
v�B
xB
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.25(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801150037142018011500371420180115003714201806221324432018062213244320180622132443201804050728022018040507280220180405072802  JA  ARFMdecpA19c                                                                20180111093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180111003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180111003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180111003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180111003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180111003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180111003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180111003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180111003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180111003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180111005600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180111153329  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180112000000  CF  PSAL_ADJUSTED_QCCj�Ct  G�O�                JM  ARCAJMQC2.0                                                                 20180114153714  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180114153714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222802  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042443  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                