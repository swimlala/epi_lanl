CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-14T00:35:29Z creation;2016-12-14T00:35:32Z conversion to V3.1;2019-12-19T08:23:12Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161214003529  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA  I2_0576_067                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��sޠ 1   @��t�� @:��n.��d����$1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p  @�  @�  A  A<  A\  A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B��B  BffB'ffB/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
�Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D�fDp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%�fD&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� Dp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�;3D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D��3D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D���D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�x DƸ D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�;3D�x Dظ D�� D�8 D�x Dٸ D���D�4�D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D��D���D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D��3D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�dZA�ffA�hsA�hsA�hsA�jA�jA�l�A�n�A�l�A�n�A�n�A�n�A�n�A�n�A�p�A�p�A�p�A�r�A�r�A�t�A�x�A�z�A�z�A�x�A�x�A�x�A�p�A�l�A�S�A��yAð!A�1'A¼jA�1A�r�A�I�A�-A�(�A�JA�O�A��jA�=qA�bA���A�bA�v�A��
A�Q�A�XA���A���A��\A�(�A�I�A�p�A���A�l�A�9XA�-A��\A��A�S�A� �A��A��jA��yA�1A�\)A�Q�A�+A�  A�l�A��HA�VA�I�A�  A���A�$�A��`A�E�A��A�VA���A��FA���A��7A�v�A�$�A��jA�`BA���A�x�A�A%A}t�A}XA}oA{VAy7LAxA�Aw%Av  Au�#Au�FAu�PAu�Au�At�9As��Aq�Ap�!Ap�ApffAp1'Ao�mAoO�AnȴAnE�AmƨAlI�Ak&�AiG�AhJAg+Ae�wAeK�Ad5?Ac&�Aa�#A_�A_VA^�A^ �A]\)A[l�AZAYoAX�AW
=AU��AUAUt�AUK�AS��AQ�TAP�AN�uANVAM�
AM`BAL~�ALAK�mAL=qAK�PAK?}AKVAJ�HAJ�+AJ-AI�AIAH�yAG�wAG��AF��AF��AF�uAFI�AFJAE��ADn�AC�wAC�AB��AA��A@��A@��A=�TA<ZA:��A:�A8��A7�A7O�A6��A6��A6ZA5��A5�#A5�PA5C�A5;dA5;dA4�RA4�A3t�A2��A1C�A0�\A/��A/O�A-�hA,(�A+��A*�A)A(ffA'�A'C�A'?}A'
=A&ZA%��A%oA$��A#�A#�A#`BA"-A!�A �Av�A�FAE�Av�A�A33A�+AhsAA�
AƨA�^AG�AȴA\)A�yA1A��Ap�AĜA+A�jAƨA�RAbA��Al�A
ZA	��A�!A1'AoA�^AA"�A=qAA�PA z�A  �@�33@�|�@�S�@���@�=q@��`@��D@�9X@�t�@�{@�hs@���@�r�@� �@�J@�b@�7@�A�@띲@�-@�ƨ@�C�@��@��H@�V@�G�@��/@�@�;d@�+@�9X@��@۾w@���@��@�p�@ץ�@�1'@�V@�5?@ёh@�9X@θR@͑h@��`@�b@ʰ!@ɺ^@ț�@�1@�l�@���@�M�@ċD@�|�@�@\@��@�x�@��9@�dZ@�~�@��@�Ĝ@�r�@�I�@�t�@�5?@�G�@�z�@���@��7@���@��u@�;d@�V@��@���@�ȴ@���@���@���@�@��@�&�@��@� �@��F@�+@�v�@��@�7L@�9X@���@�`B@���@��9@�Z@��;@�\)@��+@�5?@�G�@��u@�1'@���@�{@�@�O�@��`@�Q�@��;@�C�@�@�$�@�G�@�%@��`@��/@���@���@��j@�r�@� �@��
@��F@�;d@��@��@���@���@��/@��@���@��+@�J@��-@�`B@�/@��9@�b@��F@���@�+@���@�=q@�{@�@��@���@��T@��@�hs@�X@�O�@��@�1@��;@���@�\)@�K�@�;d@�"�@���@��@��!@��\@�ff@�-@���@�G�@���@��
@�"�@��@��!@�~�@���@�@��h@�V@���@�A�@��@��@��m@��m@��
@�t�@�@���@��\@�v�@�^5@�=q@�$�@���@�`B@���@��@��@�j@�Z@�bN@�Q�@�Z@�w@+@|�@|(�@|�@{��@{�m@{�F@{��@{�@{dZ@z��@zM�@y�@yx�@x�`@xr�@w��@vȴ@v�+@vv�@vV@vV@vE�@u��@u�T@u�h@tz�@sƨ@s��@s�F@r�@q�#@qG�@q%@p��@p�`@pĜ@pr�@p1'@pb@p �@p1'@pA�@pbN@p�@p��@pĜ@p��@p��@p�`@q7L@q&�@pA�@n��@n��@nv�@n5?@m�@m��@m?}@l��@l��@l�D@l(�@k�
@k�F@k��@k��@k��@kt�@k33@ko@j��@j�!@j��@jn�@j=q@i�@i&�@h��@h�`@h��@h�9@hb@g�@g�P@gK�@g+@g+@g
=@f�y@f�y@f�R@f�+@f{@e�T@e@e��@e�@eO�@e/@d�/@d�@dz�@dz�@d�D@dz�@d��@d�@d�@cƨ@c�@c33@b�H@b��@b��@b�!@b~�@b-@a��@a7L@`��@`bN@`1'@`b@_�@_l�@^�R@^E�@]`B@\��@\��@\z�@\Z@[33@Z�\@Y�#@WK�@W
=@V��@V�+@V@UV@T�@T��@S��@R�@S��@Sƨ@T�@Tj@TI�@S��@Sƨ@S�F@S�@SdZ@SC�@R�@R��@R�!@R~�@R=q@Q�@Q��@Qhs@Q�@P��@P�@O�@O;d@Nȴ@N�+@NE�@M�@M�@L�@L�D@Lz�@Lj@L�@K�m@K�
@K�F@KS�@J�H@J~�@J^5@JJ@I�^@I��@Ix�@IG�@I7L@I%@H�u@H1'@G��@Gl�@G+@G
=@G
=@F��@F��@Fȴ@F�+@F{@E�-@Ep�@E�@D�@D��@D9X@Cƨ@C�@CS�@C@B��@B=q@A�#@Ax�@A%@@Ĝ@@�@@ �@?��@?\)@?;d@?+@>�y@>ȴ@>�R@>v�@>5?@=��@=��@=p�@=?}@=V@<�@<�@<z�@;��@;�@;dZ@;C�@;C�@;33@;"�@;@:�H@:��@:�!@:�!@:��@:~�@:=q@9�@9�^@9��@9�@8��@8Q�@7�@7�P@7l�@7K�@7;d@7�@6ȴ@65?@6@5��@5p�@5�@4��@4I�@4�@3��@3�
@3ƨ@3��@3�@3�@3C�@3"�@3"�@3"�@3"�@3"�@3o@3@2�\@2-@2�@1��@1%@0��@01'@0  @/�w@/+@.�@.v�@.5?@-��@-`B@,�j@,�D@,Z@,(�@+�m@*��@*J@)��@)��@)�7@)hs@)X@)7L@)&�@)�@)%@)%@(�`@(�`@(Ĝ@(�9@(��@(��@(�@(bN@( �@(b@'�@'��@'�@'�P@'�P@'|�@'|�@'|�@'l�@'+@&�y@&��@&��@&��@&v�@&V@&$�@%�T@%��@%O�@$�j@$(�@#ƨ@#S�@#33@#"�@#@"�H@"�!@"��@"�\@"��@"�\@"^5@"�@!��@!�^@!G�@ Ĝ@ A�@ A�@  �@��@��@�P@|�@
=@+@
=@ȴ@�R@v�@$�@@`B@�/@�D@Z@(�@�m@dZ@C�@"�@o@@�H@��@=q@=q@�@�#@��@hs@%@�9@�u@r�@A�@1'@  @�;@��@��@\)@
=@��@v�@ff@$�@{@�T@�@`B@�@j@9X@(�@�@�m@t�@33@"�@@�H@��@�!@�\@^5@�@��@��@�7@�@��@�u@bN@b@�@�w@l�@��@�+@��@`B@/@�@V@�/@j@I�@I�@�@��@�
@�
@�
@ƨ@ƨ@ƨ@�F@�F@��@dZ@"�@
�@
��@
��@
��@
~�@
M�@
�@
�@
�@
J@	��@	�@	�^@	x�@	hs@	G�@	G�@	�@��@�9@�@r�@Q�@1'@|�@��@V@5?@@�T@��@�h@p�@`B@`B@O�@��@�@��@j@Z@I�@I�@9X@9X@9X@�@��@�
@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�dZA�ffA�hsA�hsA�hsA�jA�jA�l�A�n�A�l�A�n�A�n�A�n�A�n�A�n�A�p�A�p�A�p�A�r�A�r�A�t�A�x�A�z�A�z�A�x�A�x�A�x�A�p�A�l�A�S�A��yAð!A�1'A¼jA�1A�r�A�I�A�-A�(�A�JA�O�A��jA�=qA�bA���A�bA�v�A��
A�Q�A�XA���A���A��\A�(�A�I�A�p�A���A�l�A�9XA�-A��\A��A�S�A� �A��A��jA��yA�1A�\)A�Q�A�+A�  A�l�A��HA�VA�I�A�  A���A�$�A��`A�E�A��A�VA���A��FA���A��7A�v�A�$�A��jA�`BA���A�x�A�A%A}t�A}XA}oA{VAy7LAxA�Aw%Av  Au�#Au�FAu�PAu�Au�At�9As��Aq�Ap�!Ap�ApffAp1'Ao�mAoO�AnȴAnE�AmƨAlI�Ak&�AiG�AhJAg+Ae�wAeK�Ad5?Ac&�Aa�#A_�A_VA^�A^ �A]\)A[l�AZAYoAX�AW
=AU��AUAUt�AUK�AS��AQ�TAP�AN�uANVAM�
AM`BAL~�ALAK�mAL=qAK�PAK?}AKVAJ�HAJ�+AJ-AI�AIAH�yAG�wAG��AF��AF��AF�uAFI�AFJAE��ADn�AC�wAC�AB��AA��A@��A@��A=�TA<ZA:��A:�A8��A7�A7O�A6��A6��A6ZA5��A5�#A5�PA5C�A5;dA5;dA4�RA4�A3t�A2��A1C�A0�\A/��A/O�A-�hA,(�A+��A*�A)A(ffA'�A'C�A'?}A'
=A&ZA%��A%oA$��A#�A#�A#`BA"-A!�A �Av�A�FAE�Av�A�A33A�+AhsAA�
AƨA�^AG�AȴA\)A�yA1A��Ap�AĜA+A�jAƨA�RAbA��Al�A
ZA	��A�!A1'AoA�^AA"�A=qAA�PA z�A  �@�33@�|�@�S�@���@�=q@��`@��D@�9X@�t�@�{@�hs@���@�r�@� �@�J@�b@�7@�A�@띲@�-@�ƨ@�C�@��@��H@�V@�G�@��/@�@�;d@�+@�9X@��@۾w@���@��@�p�@ץ�@�1'@�V@�5?@ёh@�9X@θR@͑h@��`@�b@ʰ!@ɺ^@ț�@�1@�l�@���@�M�@ċD@�|�@�@\@��@�x�@��9@�dZ@�~�@��@�Ĝ@�r�@�I�@�t�@�5?@�G�@�z�@���@��7@���@��u@�;d@�V@��@���@�ȴ@���@���@���@�@��@�&�@��@� �@��F@�+@�v�@��@�7L@�9X@���@�`B@���@��9@�Z@��;@�\)@��+@�5?@�G�@��u@�1'@���@�{@�@�O�@��`@�Q�@��;@�C�@�@�$�@�G�@�%@��`@��/@���@���@��j@�r�@� �@��
@��F@�;d@��@��@���@���@��/@��@���@��+@�J@��-@�`B@�/@��9@�b@��F@���@�+@���@�=q@�{@�@��@���@��T@��@�hs@�X@�O�@��@�1@��;@���@�\)@�K�@�;d@�"�@���@��@��!@��\@�ff@�-@���@�G�@���@��
@�"�@��@��!@�~�@���@�@��h@�V@���@�A�@��@��@��m@��m@��
@�t�@�@���@��\@�v�@�^5@�=q@�$�@���@�`B@���@��@��@�j@�Z@�bN@�Q�@�Z@�w@+@|�@|(�@|�@{��@{�m@{�F@{��@{�@{dZ@z��@zM�@y�@yx�@x�`@xr�@w��@vȴ@v�+@vv�@vV@vV@vE�@u��@u�T@u�h@tz�@sƨ@s��@s�F@r�@q�#@qG�@q%@p��@p�`@pĜ@pr�@p1'@pb@p �@p1'@pA�@pbN@p�@p��@pĜ@p��@p��@p�`@q7L@q&�@pA�@n��@n��@nv�@n5?@m�@m��@m?}@l��@l��@l�D@l(�@k�
@k�F@k��@k��@k��@kt�@k33@ko@j��@j�!@j��@jn�@j=q@i�@i&�@h��@h�`@h��@h�9@hb@g�@g�P@gK�@g+@g+@g
=@f�y@f�y@f�R@f�+@f{@e�T@e@e��@e�@eO�@e/@d�/@d�@dz�@dz�@d�D@dz�@d��@d�@d�@cƨ@c�@c33@b�H@b��@b��@b�!@b~�@b-@a��@a7L@`��@`bN@`1'@`b@_�@_l�@^�R@^E�@]`B@\��@\��@\z�@\Z@[33@Z�\@Y�#@WK�@W
=@V��@V�+@V@UV@T�@T��@S��@R�@S��@Sƨ@T�@Tj@TI�@S��@Sƨ@S�F@S�@SdZ@SC�@R�@R��@R�!@R~�@R=q@Q�@Q��@Qhs@Q�@P��@P�@O�@O;d@Nȴ@N�+@NE�@M�@M�@L�@L�D@Lz�@Lj@L�@K�m@K�
@K�F@KS�@J�H@J~�@J^5@JJ@I�^@I��@Ix�@IG�@I7L@I%@H�u@H1'@G��@Gl�@G+@G
=@G
=@F��@F��@Fȴ@F�+@F{@E�-@Ep�@E�@D�@D��@D9X@Cƨ@C�@CS�@C@B��@B=q@A�#@Ax�@A%@@Ĝ@@�@@ �@?��@?\)@?;d@?+@>�y@>ȴ@>�R@>v�@>5?@=��@=��@=p�@=?}@=V@<�@<�@<z�@;��@;�@;dZ@;C�@;C�@;33@;"�@;@:�H@:��@:�!@:�!@:��@:~�@:=q@9�@9�^@9��@9�@8��@8Q�@7�@7�P@7l�@7K�@7;d@7�@6ȴ@65?@6@5��@5p�@5�@4��@4I�@4�@3��@3�
@3ƨ@3��@3�@3�@3C�@3"�@3"�@3"�@3"�@3"�@3o@3@2�\@2-@2�@1��@1%@0��@01'@0  @/�w@/+@.�@.v�@.5?@-��@-`B@,�j@,�D@,Z@,(�@+�m@*��@*J@)��@)��@)�7@)hs@)X@)7L@)&�@)�@)%@)%@(�`@(�`@(Ĝ@(�9@(��@(��@(�@(bN@( �@(b@'�@'��@'�@'�P@'�P@'|�@'|�@'|�@'l�@'+@&�y@&��@&��@&��@&v�@&V@&$�@%�T@%��@%O�@$�j@$(�@#ƨ@#S�@#33@#"�@#@"�H@"�!@"��@"�\@"��@"�\@"^5@"�@!��@!�^@!G�@ Ĝ@ A�@ A�@  �@��@��@�P@|�@
=@+@
=@ȴ@�R@v�@$�@@`B@�/@�D@Z@(�@�m@dZ@C�@"�@o@@�H@��@=q@=q@�@�#@��@hs@%@�9@�u@r�@A�@1'@  @�;@��@��@\)@
=@��@v�@ff@$�@{@�T@�@`B@�@j@9X@(�@�@�m@t�@33@"�@@�H@��@�!@�\@^5@�@��@��@�7@�@��@�u@bN@b@�@�w@l�@��@�+@��@`B@/@�@V@�/@j@I�@I�@�@��@�
@�
@�
@ƨ@ƨ@ƨ@�F@�F@��@dZ@"�@
�@
��@
��@
��@
~�@
M�@
�@
�@
�@
J@	��@	�@	�^@	x�@	hs@	G�@	G�@	�@��@�9@�@r�@Q�@1'@|�@��@V@5?@@�T@��@�h@p�@`B@`B@O�@��@�@��@j@Z@I�@I�@9X@9X@9X@�@��@�
@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BVBVBVBVBVBVBVBVBVBVB\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\BbBoB�B"�B,B=qB@�BD�BB�B;dB&�B\B  B�B�B�/B��B��B��BǮBÖB��B�9B��B��B�=B�1B�+B�B|�Br�Bk�B^5BN�BC�B5?B �B�B�BVB��B�NB�BĜB�FB��B~�BaHBXB?}B/B�B\B	7B
��B
�B
�fB
�)B
��B
��B
��B
��B
��B
��B
ɺB
ǮB
B
�3B
�B
��B
��B
��B
�uB
�DB
y�B
r�B
jB
dZB
aHB
`BB
_;B
_;B
^5B
ZB
Q�B
E�B
=qB
;dB
:^B
9XB
7LB
33B
/B
,B
'�B
�B
�B

=B
B	��B	��B	�B	�yB	�HB	�B	��B	ÖB	��B	�jB	�?B	��B	��B	�oB	�VB	�B	{�B	� B	�B	�B	}�B	t�B	bNB	O�B	I�B	D�B	=qB	7LB	5?B	7LB	T�B	O�B	M�B	L�B	O�B	M�B	K�B	K�B	J�B	H�B	?}B	>wB	8RB	5?B	2-B	2-B	2-B	/B	&�B	�B	�B	uB	
=B	%B	uB��B�B�ZB�mB�HB�B��B��B��B��B��B��B��B��B�#B�HB�HB�;B�)B�B�B��B��B��BƨB�}B�}B�jB�XB�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B�{B�VB�DB�1B}�Bz�Bx�Bw�Bu�Br�Bq�Bp�Bo�Bm�Bl�BiyBgmBdZBcTBbNBcTB]/B[#BYBVBS�BR�BP�BO�BM�BJ�BH�BF�BB�B?}B>wB;dB;dB;dB<jB<jB?}B>wB>wB>wB?}BB�BD�BE�BF�BG�BF�BF�BF�BE�BE�BB�B<jB8RB8RB7LB<jB>wB?}B@�B@�BA�BB�BC�BC�BB�BB�BA�B?}B<jB9XB5?B0!B#�B�B�B�B�B�BuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B'�B'�B(�B.B.B0!B1'B.B-B,B1'B8RB:^B:^B;dB;dB@�BA�BA�BB�BD�BE�BE�BD�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BF�BJ�BN�BQ�BW
BYB\)B_;B`BBgmBjBl�Bm�Bp�Bs�Bu�Bw�Bx�B}�B�B�1B�7B�DB�DB�DB�JB�PB�VB�VB�VB�hB��B��B��B��B��B�B�B�-B�9B�LB�XB�^B�jB��B��BBŢBɺB��B��B��B��B��B��B�B�
B�
B�
B�/B�NB�TB�`B�mB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	%B	VB	{B	�B	�B	�B	�B	�B	�B	#�B	&�B	'�B	(�B	+B	,B	1'B	33B	6FB	9XB	;dB	;dB	<jB	=qB	?}B	C�B	G�B	G�B	P�B	S�B	S�B	T�B	T�B	W
B	XB	ZB	ZB	_;B	bNB	dZB	ffB	iyB	k�B	p�B	s�B	u�B	w�B	x�B	}�B	�B	�B	�B	�1B	�DB	�PB	�VB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�FB	�FB	�9B	�9B	�3B	�9B	�?B	�FB	�LB	�LB	�RB	�^B	�jB	�qB	�qB	�wB	�wB	��B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�HB	�NB	�NB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B

=B

=B
DB
JB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
hB
hB
hB
oB
uB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
(�B
(�B
(�B
(�B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
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
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
B�B
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
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
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
`BB
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
cTB
cTB
cTB
dZB
dZB
dZB
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
hsB
hsB
hsB
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
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BpBpBpBpBpBpBVBpBpBpBvBvBvBvBvB\BvBvBvBvBvBvBvBvBvBvB}B�B�B#TB-B>BBA�BF%BEB?�B-CB�B�B��B��B�'B� B͟B�0B��B�zB�mB�lB�/B��B��B�B��B�3BBuBn}Ba-BQ�BGEB8�B"B	B�BuB��B�,B��B�B�JB�jB�[Bc�B[qBBAB1�BB B�B
�^B
��B
�B
�OB
յB
�NB
�HB
�\B
��B
�B
�B
ɺB
��B
��B
�qB
�fB
�1B
��B
��B
�PB
{dB
tB
k�B
d�B
a�B
`�B
_�B
_�B
_pB
[�B
S�B
F�B
=�B
;�B
:�B
9�B
8B
4B
0B
-)B
)�B
 vB
�B
�B
uB	��B	��B	�'B	�B	� B	�7B	��B	�gB	��B	��B	��B	��B	��B	�[B	�bB	�tB	|jB	��B	��B	�B	��B	v�B	c�B	P}B	J�B	EmB	>�B	7�B	5�B	7�B	U�B	PbB	N<B	MPB	PbB	NpB	LJB	LdB	K�B	I�B	@ B	?HB	8�B	5�B	2�B	2�B	3B	0�B	'�B	 �B	B	�B	�B	zB	�B	 B�B�B�B�B��BյB�uB�hB�}B�BB�pB�PB�(B�qB�B�NB�\BݘB��B�$B�B�HB�B�fB��B��B�(B��B�B��B�iB��B�/B�"B��B��B��B�bB��B�\B��B�_B��B��B�jB�XBcB{�BzByrBwLBsBq�BqBp�Bn�BnIBjBh�Be,BdBc�Be,B^5B\�BZ�BW$BT�BS�BR�BQBO(BK�BJrBH�BDBA�B?�B<B<jB<�B=<B=�BA�B>�B>�B?HB@�BCBEBFtBG�BHfBG+BG_BG_BGEBGEBD3B=�B9>B9�B8�B<�B>�B?�BA;BAUBBBCGBD�BD�BDMBD3BCB@OB=<B:DB6�B2aB%B#B�B�B�BmB,B{B�BsB�B+B1B+ByB�BeBBB1BKBxB"�B(�B(�B)�B.}B.�B0�B2-B/ B-�B-wB2-B8�B;B;�B<�B<�B@�BA�BA�BB�BEBF?BF%BEBF%BF?BF%BF?BFYBFYBFtBF�BF�BF�BF?BFBG+BKDBO�BR�BW�BZB\�B_�BaHBh
Bj�BmBn/BqABt9Bv`BxRBy�B~�B�gB��B�lB��B�xB��B��B��B��B��B�(B��B�SB��B��B�B�fB��B��B��B��B��B��B��B�"B��B��B�B�%B�=B�"B�.B�B�B�:B�aB�9B�?B�YB��BݲB�B�B�B�B�B��B��B��B��B��B��B� B�!B�B�aB�B�TB�B�%B�%B�FB�*B�6B�qB	�B	�B	�B	�B	�B	�B	�B	B	B	 B	$&B	'8B	($B	)DB	+QB	,�B	1vB	3�B	6�B	9�B	;�B	;�B	<�B	=�B	?�B	D3B	H1B	H�B	QNB	TB	T,B	UMB	U2B	W?B	X_B	ZQB	Z�B	_�B	b�B	d�B	f�B	i�B	lB	qB	s�B	vB	xB	y	B	~BB	�;B	�AB	�mB	��B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�
B	�B	�0B	�=B	�OB	��B	��B	��B	��B	�nB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	��B	�)B	�B	�(B	�B	�B	� B	�B	�4B	�4B	�B	�:B	�TB	�,B	�2B	�9B	�?B	�_B	�KB	�KB	�QB	�WB	�IB	�OB	�VB	�bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�'B	��B	��B	��B	�B	�-B	�?B	�LB	��B	��B	��B	�B	�B	�3B	��B	�B	�B	�2B	��B	��B	�B	�(B	�.B
aB
MB
gB
MB
mB
SB
SB
mB
mB
_B
�B
fB
	�B
	lB

�B

rB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
�B
 B
 B
!B
!B
"B
"4B
#B
$B
$@B
%,B
%B
&2B
&B
&B
'8B
'B
'B
'B
'8B
(>B
(>B
(>B
)*B
)DB
)*B
)*B
*KB
+6B
,=B
,=B
,"B
,"B
,=B
,=B
-CB
-)B
-CB
-CB
-CB
-CB
-CB
.IB
.IB
.IB
.cB
/�B
0UB
0�B
0UB
0UB
1[B
1[B
1[B
1vB
1�B
2|B
3hB
3�B
4nB
4nB
4�B
5tB
5�B
5tB
6`B
6zB
6�B
6`B
6�B
6zB
6`B
6`B
6`B
7fB
7�B
7�B
7�B
7�B
8�B
8�B
9�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
>�B
?�B
@�B
@�B
@�B
@�B
AB
B�B
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
F�B
G�B
G�B
G�B
G�B
G�B
IB
H�B
H�B
IB
IB
J#B
KB
KB
K�B
K�B
L�B
MB
MB
MB
L�B
MB
L�B
MB
MB
N"B
NB
N"B
O(B
O(B
P.B
O�B
PB
PB
QB
QB
QB
Q4B
RB
QB
R B
R:B
R B
R:B
S@B
S@B
TFB
T,B
U2B
U2B
U2B
UMB
V9B
VSB
VB
VSB
V9B
WYB
W?B
W$B
W?B
XEB
XEB
X_B
YKB
YeB
YKB
ZQB
ZQB
ZQB
ZQB
ZQB
[WB
[qB
[qB
[WB
\xB
\]B
\]B
]dB
]dB
]~B
]~B
^�B
^�B
_pB
_pB
_VB
_�B
_pB
_�B
`vB
`\B
`vB
a�B
a|B
a|B
a�B
a|B
a�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
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
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
nB
o B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.25(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612180036162016121800361620161218003616201806221218142018062212181420180622121814201804050411202018040504112020180405041120  JA  ARFMdecpA19c                                                                20161214093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161214003529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161214003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161214003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161214003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161214003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161214003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161214003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161214003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161214003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20161214013037                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161214153408  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161217153616  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161217153616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191120  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031814  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                