CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:23Z creation;2016-06-24T09:48:25Z conversion to V3.1;2019-12-19T08:38:25Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20160624094823  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_006                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׳��� 1   @׳��`�@;��	��di5�Xy>1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƃ3D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @vff@�  @�  A  A<  A\  A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� CٚC� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9�fC;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_ٚCa� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&�fD'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dup Du� Dvp Dv� Dwp Dw� Dxp Dx� Dyp Dy� Dzp Dz� D{p D{� D|p D|� D}p D}� D~p D~� Dp D� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�;3D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�;3D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D¸ D�� D�8 D�x Dø D�� D�8 D�x Dĸ D�� D�8 D�x DŸ D�� D�8 D�{3Dƻ3D�� D�8 D�x DǸ D�� D�8 D�x Dȸ D�� D�8 D�x Dɸ D�� D�8 D�x Dʸ D�� D�8 D�x D˸ D�� D�8 D�x D̸ D�� D�8 D�x D͸ D�� D�8 D�x Dθ D�� D�8 D�x Dϸ D�� D�8 D�x Dи D�� D�8 D�x DѸ D�� D�8 D�x DҸ D�� D�8 D�x DӸ D�� D�8 D�x DԸ D�� D�8 D�x Dո D�� D�8 D�x Dָ D�� D�8 D�x D׸ D�� D�;3D�x Dظ D�� D�8 D�x Dٸ D�� D�8 D�x Dڸ D�� D�8 D�x D۸ D�� D�8 D�x Dܸ D�� D�8 D�x Dݸ D�� D�8 D�x D޸ D�� D�8 D�x D߸ D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D� D���D�8 D�x D� D�� D�8 D�x D� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�x D�� D�� D�8 D�t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ĜA���A��A��-A���A���A�$�A�ffA�VA��A�S�A�ĜA�t�A�7LA�hsA�S�A��A��;A��RA��!A�VA�Q�A��A��/A�~�A�E�A��A�I�A��jA��wA���A�33A��#A�A�C�A�A�A�p�A��RA��/A��A�G�A��A�r�A�
=A��-A�z�A�VA���A��A�;dA�  A��PA�I�A��#A��+A�A�A��/A�bA��A�7LA�O�A��jA��+A��#A���A�hsA��hA��A�x�A�"�A��A�S�A��-A���A��uA�9XA�|�A��A�O�A��A�bA��FA�ffA�$�A��jA�S�A��A~��A~A}��A}��A}|�A}33A{33Aw�Avz�Au�^AtQ�ArAnr�Ak�#AjVAiO�AhĜAhbNAhbAf(�Ae�Ad^5Ac�
Ac;dAb �Aa�^AaVA`(�A^��A^�A]��A]"�A\��A[�#AZĜAYp�AY
=AX��AW��AW��AWK�AW�AV�9AUAUl�AT�yAT^5ATbAS`BARbNAO�AOdZAN�!AL�uAK�hAJJAH�`AGƨAG�7AF��AE|�ADJACt�AB��AA7LA@M�A@�A@(�A?��A@  A>ĜA=7LA<E�A;XA:�!A:��A9��A8bA7VA6-A5O�A4�A3�TA2�`A2��A1"�A/�PA.9XA-\)A,��A+�;A+�FA+&�A*�RA)�FA(v�A&��A&ZA%�
A%�A%XA$�A#��A"^5A!l�A ZAhsAVA^5A/AjA�TA�7AC�A�A �AO�A��AA��AM�A�-AoA�`A�!AjA��A�An�A  A��A33A&�A+A/A+AĜA��A�A=qA|�A�A=qA��A�A
E�A	�AQ�A�AbA��A  A��A��A~�Ar�A^5A�PA  �@��w@�|�@��H@���@�O�@�Z@���@�j@���@��;@�|�@�@� �@�+@�@�V@�+@�~�@�z�@�M�@噚@��/@��m@�P@�o@���@��@߾w@�S�@�"�@�E�@�J@��T@�V@� �@۾w@���@�C�@Ձ@ӶF@�;d@�
=@җ�@Ѳ-@�A�@�33@�G�@�n�@��#@�`B@��@ư!@���@ř�@ŉ7@�`B@���@��
@��@��m@��#@��/@�bN@�  @���@�33@��@���@�n�@��h@��/@�r�@�ƨ@���@�j@��;@��@��H@��T@��@��u@���@�t�@��@��y@��^@�r�@�  @��;@��F@��H@�Ĝ@�1'@��w@��P@�dZ@��@��7@�b@��@�|�@���@�V@�J@��-@�x�@�&�@���@��/@�j@� �@��;@���@�E�@��-@�G�@��;@��R@��T@��@��u@�j@�Z@�Q�@�9X@��@��@��
@��@�;d@��@�@�ȴ@���@��@���@�p�@�`B@�V@���@��9@�r�@�K�@���@�=q@�{@��#@���@���@��@�X@��j@�1@��P@�
=@���@��@��#@���@�p�@�&�@��/@��@���@���@��u@�A�@�1@���@�dZ@�"�@��H@���@�$�@���@�hs@��@�j@�I�@� �@��;@�ƨ@��@��@�dZ@���@��R@�E�@���@�`B@��@��u@�r�@�Q�@�9X@��@�b@�  @�  @�;@��@|�@K�@�@~��@~�+@~E�@}�@}p�@}/@|�/@|z�@|I�@|1@z�\@yX@xQ�@w�@w�@v�y@vȴ@v��@vv�@vE�@v@u��@u�-@u�h@uO�@t�/@t1@st�@s"�@r�@r�\@r�@q��@q��@q�7@q%@p�9@pbN@p �@p �@o�@oK�@n��@n$�@n{@m�@m�@m�T@m��@m�-@mV@l��@lz�@lZ@l(�@k�
@kC�@j�!@j~�@jn�@j^5@jM�@jJ@i�^@i��@i%@hr�@h1'@g�@g�P@g|�@g;d@fȴ@e�T@e��@ep�@d��@d9X@d1@c��@cC�@b��@b~�@b=q@a�@a�^@ahs@a&�@`�`@`�@`r�@`Q�@`1'@`b@_��@_��@^��@^��@^V@^@\��@\��@\�@\��@\�D@\z�@\j@\9X@\�@[�m@[ƨ@[��@[C�@Z�!@Z=q@Z-@Y��@Y�@X�`@X��@X�@Xr�@XQ�@W�w@WK�@W
=@Vȴ@VV@Vff@V@U`B@U?}@U/@U/@T�@T��@T�@Tj@T9X@T1@S��@S�m@S�
@S�F@S��@S�@SC�@S@R��@R~�@Q��@Q�7@Q�7@QX@Q�@Q�@P�`@Q%@P��@P�`@P��@PĜ@P�9@P�u@P�@PA�@O��@O;d@Nȴ@N��@N��@N��@N��@Nv�@N$�@M�@M�-@MV@L�D@LZ@L1@Kƨ@Kƨ@Kƨ@K��@K��@K��@K�@Kt�@KS�@KC�@K"�@K"�@Ko@J�@J�!@J~�@J^5@JM�@J�@Ihs@I�@H��@H�`@HĜ@H��@H�u@HQ�@HA�@HQ�@H1'@H �@H  @G�w@G�@G|�@G;d@F�@F��@Fv�@FE�@F{@E�@E�-@E�-@E�-@E��@E�@Ep�@E`B@E�@D�j@D1@Ct�@C"�@C@B�@B��@B�@A�7@A%@@�u@@b@?�@?\)@?+@>��@>ȴ@>�+@>E�@=�@=��@=�h@=p�@=?}@<��@<�/@<��@<Z@<I�@<(�@;�m@;��@;dZ@;"�@:�\@:M�@:-@9��@9�#@9�^@9�7@9&�@8��@8bN@8  @7�@7|�@7
=@6��@6ȴ@6�+@6ff@6V@6@5��@5@5�-@5p�@5V@4�/@4��@4�D@4Z@3�m@3ƨ@3C�@2~�@2M�@2-@1��@1�@1��@1��@1x�@1&�@0�9@0Q�@/�@/+@.�y@.E�@-�@-@-��@-�h@-p�@-?}@-�@,�@,�@,�@,�D@+��@+@*��@*~�@*^5@*=q@)��@)��@)hs@)G�@)G�@)&�@(Ĝ@(��@(�u@(�@(r�@(r�@( �@'��@'�P@'l�@'\)@'�@&��@&�y@&�y@&�R@&�+@&ff@%�@%?}@$�/@$�j@$��@$�D@$j@$9X@$(�@$�@#��@#�F@#��@#S�@#o@"�H@"��@"��@"�!@"~�@"n�@"^5@"=q@"-@"�@"�@"J@!�#@!��@!G�@!�@ ��@ �`@ Ĝ@ �u@ �@ r�@ r�@ 1'@�@�P@\)@�@��@�@ȴ@ȴ@ȴ@ȴ@��@�+@�+@V@E�@5?@{@@@�h@O�@�j@�@�
@ƨ@�F@�@C�@@�H@�H@��@��@��@�!@n�@J@��@G�@&�@%@�@�@��@Ĝ@�u@bN@1'@ �@  @�@�@��@�w@\)@
=@��@�+@$�@{@{@�@�-@O�@?}@V@��@�j@��@Z@9X@(�@�@1@�
@��@t�@C�@33@@��@��@��@�\@�\@^5@�@��@X@Ĝ@Ĝ@�9@Q�@�@�w@��@�P@�@�R@��@�+@v�@ff@V@{@�@�@��@�-@��@�@p�@O�@��@��@z�@Z@(�@��@��@S�@"�@
��@
��@
�\@
^5@
=q@
-@	��@	�@	�@	�@	�@	�#@	�^@	x�@	X@	X@	X@	�@�`@Ĝ@r�@A�@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ĜA���A��A��-A���A���A�$�A�ffA�VA��A�S�A�ĜA�t�A�7LA�hsA�S�A��A��;A��RA��!A�VA�Q�A��A��/A�~�A�E�A��A�I�A��jA��wA���A�33A��#A�A�C�A�A�A�p�A��RA��/A��A�G�A��A�r�A�
=A��-A�z�A�VA���A��A�;dA�  A��PA�I�A��#A��+A�A�A��/A�bA��A�7LA�O�A��jA��+A��#A���A�hsA��hA��A�x�A�"�A��A�S�A��-A���A��uA�9XA�|�A��A�O�A��A�bA��FA�ffA�$�A��jA�S�A��A~��A~A}��A}��A}|�A}33A{33Aw�Avz�Au�^AtQ�ArAnr�Ak�#AjVAiO�AhĜAhbNAhbAf(�Ae�Ad^5Ac�
Ac;dAb �Aa�^AaVA`(�A^��A^�A]��A]"�A\��A[�#AZĜAYp�AY
=AX��AW��AW��AWK�AW�AV�9AUAUl�AT�yAT^5ATbAS`BARbNAO�AOdZAN�!AL�uAK�hAJJAH�`AGƨAG�7AF��AE|�ADJACt�AB��AA7LA@M�A@�A@(�A?��A@  A>ĜA=7LA<E�A;XA:�!A:��A9��A8bA7VA6-A5O�A4�A3�TA2�`A2��A1"�A/�PA.9XA-\)A,��A+�;A+�FA+&�A*�RA)�FA(v�A&��A&ZA%�
A%�A%XA$�A#��A"^5A!l�A ZAhsAVA^5A/AjA�TA�7AC�A�A �AO�A��AA��AM�A�-AoA�`A�!AjA��A�An�A  A��A33A&�A+A/A+AĜA��A�A=qA|�A�A=qA��A�A
E�A	�AQ�A�AbA��A  A��A��A~�Ar�A^5A�PA  �@��w@�|�@��H@���@�O�@�Z@���@�j@���@��;@�|�@�@� �@�+@�@�V@�+@�~�@�z�@�M�@噚@��/@��m@�P@�o@���@��@߾w@�S�@�"�@�E�@�J@��T@�V@� �@۾w@���@�C�@Ձ@ӶF@�;d@�
=@җ�@Ѳ-@�A�@�33@�G�@�n�@��#@�`B@��@ư!@���@ř�@ŉ7@�`B@���@��
@��@��m@��#@��/@�bN@�  @���@�33@��@���@�n�@��h@��/@�r�@�ƨ@���@�j@��;@��@��H@��T@��@��u@���@�t�@��@��y@��^@�r�@�  @��;@��F@��H@�Ĝ@�1'@��w@��P@�dZ@��@��7@�b@��@�|�@���@�V@�J@��-@�x�@�&�@���@��/@�j@� �@��;@���@�E�@��-@�G�@��;@��R@��T@��@��u@�j@�Z@�Q�@�9X@��@��@��
@��@�;d@��@�@�ȴ@���@��@���@�p�@�`B@�V@���@��9@�r�@�K�@���@�=q@�{@��#@���@���@��@�X@��j@�1@��P@�
=@���@��@��#@���@�p�@�&�@��/@��@���@���@��u@�A�@�1@���@�dZ@�"�@��H@���@�$�@���@�hs@��@�j@�I�@� �@��;@�ƨ@��@��@�dZ@���@��R@�E�@���@�`B@��@��u@�r�@�Q�@�9X@��@�b@�  @�  @�;@��@|�@K�@�@~��@~�+@~E�@}�@}p�@}/@|�/@|z�@|I�@|1@z�\@yX@xQ�@w�@w�@v�y@vȴ@v��@vv�@vE�@v@u��@u�-@u�h@uO�@t�/@t1@st�@s"�@r�@r�\@r�@q��@q��@q�7@q%@p�9@pbN@p �@p �@o�@oK�@n��@n$�@n{@m�@m�@m�T@m��@m�-@mV@l��@lz�@lZ@l(�@k�
@kC�@j�!@j~�@jn�@j^5@jM�@jJ@i�^@i��@i%@hr�@h1'@g�@g�P@g|�@g;d@fȴ@e�T@e��@ep�@d��@d9X@d1@c��@cC�@b��@b~�@b=q@a�@a�^@ahs@a&�@`�`@`�@`r�@`Q�@`1'@`b@_��@_��@^��@^��@^V@^@\��@\��@\�@\��@\�D@\z�@\j@\9X@\�@[�m@[ƨ@[��@[C�@Z�!@Z=q@Z-@Y��@Y�@X�`@X��@X�@Xr�@XQ�@W�w@WK�@W
=@Vȴ@VV@Vff@V@U`B@U?}@U/@U/@T�@T��@T�@Tj@T9X@T1@S��@S�m@S�
@S�F@S��@S�@SC�@S@R��@R~�@Q��@Q�7@Q�7@QX@Q�@Q�@P�`@Q%@P��@P�`@P��@PĜ@P�9@P�u@P�@PA�@O��@O;d@Nȴ@N��@N��@N��@N��@Nv�@N$�@M�@M�-@MV@L�D@LZ@L1@Kƨ@Kƨ@Kƨ@K��@K��@K��@K�@Kt�@KS�@KC�@K"�@K"�@Ko@J�@J�!@J~�@J^5@JM�@J�@Ihs@I�@H��@H�`@HĜ@H��@H�u@HQ�@HA�@HQ�@H1'@H �@H  @G�w@G�@G|�@G;d@F�@F��@Fv�@FE�@F{@E�@E�-@E�-@E�-@E��@E�@Ep�@E`B@E�@D�j@D1@Ct�@C"�@C@B�@B��@B�@A�7@A%@@�u@@b@?�@?\)@?+@>��@>ȴ@>�+@>E�@=�@=��@=�h@=p�@=?}@<��@<�/@<��@<Z@<I�@<(�@;�m@;��@;dZ@;"�@:�\@:M�@:-@9��@9�#@9�^@9�7@9&�@8��@8bN@8  @7�@7|�@7
=@6��@6ȴ@6�+@6ff@6V@6@5��@5@5�-@5p�@5V@4�/@4��@4�D@4Z@3�m@3ƨ@3C�@2~�@2M�@2-@1��@1�@1��@1��@1x�@1&�@0�9@0Q�@/�@/+@.�y@.E�@-�@-@-��@-�h@-p�@-?}@-�@,�@,�@,�@,�D@+��@+@*��@*~�@*^5@*=q@)��@)��@)hs@)G�@)G�@)&�@(Ĝ@(��@(�u@(�@(r�@(r�@( �@'��@'�P@'l�@'\)@'�@&��@&�y@&�y@&�R@&�+@&ff@%�@%?}@$�/@$�j@$��@$�D@$j@$9X@$(�@$�@#��@#�F@#��@#S�@#o@"�H@"��@"��@"�!@"~�@"n�@"^5@"=q@"-@"�@"�@"J@!�#@!��@!G�@!�@ ��@ �`@ Ĝ@ �u@ �@ r�@ r�@ 1'@�@�P@\)@�@��@�@ȴ@ȴ@ȴ@ȴ@��@�+@�+@V@E�@5?@{@@@�h@O�@�j@�@�
@ƨ@�F@�@C�@@�H@�H@��@��@��@�!@n�@J@��@G�@&�@%@�@�@��@Ĝ@�u@bN@1'@ �@  @�@�@��@�w@\)@
=@��@�+@$�@{@{@�@�-@O�@?}@V@��@�j@��@Z@9X@(�@�@1@�
@��@t�@C�@33@@��@��@��@�\@�\@^5@�@��@X@Ĝ@Ĝ@�9@Q�@�@�w@��@�P@�@�R@��@�+@v�@ff@V@{@�@�@��@�-@��@�@p�@O�@��@��@z�@Z@(�@��@��@S�@"�@
��@
��@
�\@
^5@
=q@
-@	��@	�@	�@	�@	�@	�#@	�^@	x�@	X@	X@	X@	�@�`@Ĝ@r�@A�@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�!B�!B�!B�!B�!B�!B�!B�!B�!B�!B�!B�!B�B��Br�BXBO�BN�BN�BJ�BI�BG�BF�BE�B=qB6FB33B0!B.B$�B�BPB%B��B�B�B�sB�`B�TB��B��BȴB��BɺB�dB�-B��B�\B�Bu�Bk�BbNBM�BI�BD�B>wB8RB2-B/B(�B!�B�BJB+BB��B��B�B�B�`B�B��B�B��B�PB�1B{�Bz�B{�Bo�BdZB\)BE�B8RB1'BbB
��B
��B
�B
�mB
�HB
�B
�B
��B
��B
ȴB
ŢB
�qB
�FB
�B
��B
��B
��B
��B
��B
��B
�JB
l�B
cTB
`BB
VB
=qB
�B	��B	�B	�mB	�NB	�;B	�)B	��B	��B	ɺB	ɺB	��B	ȴB	ǮB	ŢB	��B	�XB	�9B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�oB	�PB	�7B	�B	}�B	{�B	t�B	l�B	\)B	T�B	P�B	<jB	1'B	,B	(�B	#�B	 �B	�B	�B	�B	�B	�B	DB	B	B	1B	
=B	�B	PB��B�B�B�fB�B�mB�B��B��BȴBƨBǮBÖBȴB�}B�'B�B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�VB�PB�=B�B�B}�Bx�Bw�Bv�Bs�Bq�Bp�Bn�Bm�Bl�BjBhsBe`BdZBaHB_;B]/B[#BZBYBXB]/BR�BO�BM�BL�BL�BM�BN�BO�BP�BVBVBVBS�BQ�BL�BK�BK�BH�BG�BD�BA�BA�B>wB;dB:^B7LB6FB5?B5?B49B49B1'B/B/B.B-B-B+B)�B(�B&�B$�B$�B#�B$�B"�B!�B �B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B�B�B!�B&�B&�B)�B.B/B0!B0!B1'B2-B2-B2-B33B5?B6FB6FB8RB=qB?}B?}B?}B@�BB�BC�BE�BG�BI�BJ�BK�BP�BT�BW
BW
BW
BYB]/B^5B_;B`BB`BBaHBgmBl�Bm�Bn�Bq�Bs�Bt�Bu�Bv�Bx�By�Bz�B|�B~�B� B�B�%B�1B�=B�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�?B�LB�RB�RB�^B�jB�jB�wBŢBɺB��B��B��B��B��B��B��B�B�#B�;B�NB�sB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	+B	DB	VB	\B	bB	hB	hB	oB	uB	uB	�B	�B	�B	#�B	%�B	)�B	.B	/B	0!B	1'B	2-B	33B	49B	49B	49B	6FB	6FB	7LB	8RB	8RB	:^B	;dB	=qB	@�B	A�B	C�B	F�B	G�B	I�B	O�B	S�B	W
B	XB	[#B	[#B	[#B	\)B	\)B	]/B	^5B	_;B	`BB	`BB	aHB	cTB	gmB	jB	k�B	k�B	m�B	o�B	q�B	q�B	r�B	u�B	w�B	y�B	z�B	z�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�JB	�PB	�PB	�hB	�hB	�oB	�oB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�?B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
bB
hB
hB
oB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
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
,B
,B
-B
,B
-B
-B
-B
-B
-B
.B
.B
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
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
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
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
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
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
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
VB
VB
VB
W
B
W
B
XB
XB
XB
YB
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
ZB
ZB
ZB
[#B
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
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
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
gmB
gmB
gmB
gmB
gmB
gmB
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
k�B
l�B
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
m�B
m�B
m�B
m�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�/B�;B�;B�;B�;B�;B�;B�;B�;B�UB�oB��B��B�B��By�BZ�BPHBOvBP.BL�BK�BH�BHBF�B>]B7�B5�B33B1�B(�BeB�B	B��B�iB�WB�B�B��B�aB��B�lB��B�)B��B��B�B��B�Bw�Bm�Bd�BN�BJ�BE�B?}B9>B2�B0UB*KB#�B�BBKB�B��B��B��B��B�B��BΊB�B�	B�<B��B|�B{�B}�Bq[BfB^�BG+B:B4TBB 4B
�B
�aB
��B
�B
ںB
ևB
��B
��B
ɠB
��B
��B
��B
�5B
��B
�BB
��B
�#B
��B
�eB
��B
nB
d�B
b�B
Y1B
AoB
B	��B	��B	�>B	�B	�BB	�OB	�FB	��B	ʦB	��B	�B	ɆB	��B	��B	�B	�^B	�B	��B	��B	�*B	�TB	�BB	�jB	�WB	��B	�1B	�B	��B	�FB	��B	��B	�	B	��B	~�B	}"B	v�B	oB	]dB	V�B	S[B	>B	33B	-�B	*KB	$�B	"4B	jB	jB	�B	B	+B	dB	{B	SB	�B	
�B	$B	BB	 OB��B�qB�B��B�yBۦB�FB�BɠB�1B�BĜB��B��B��B�WB�B�B�tB��B��B�vB��B��B�FB�:B� B��B�vB�(B��B��B��B.By�By	BxRBt�Br|BqABo5BncBm�Bk�BiyBf�Be�BbhB`BB^B[�BZ�BZBY�B_�BTBP�BNVBM�BMBN"BOBPHBQ�BVmBVmBV�BUMBS&BM�BL�BMBJ#BIRBE�BCBC�B@OB<�B;�B7�B6�B5�B5�B5�B5�B1�B/�B/�B.�B./B./B,�B+�B+B'�B%�B%�B%�B&B#�B"�B"B!�B!HB"4BpBjBpB5BjB�B�B~B)B)B]BB=BqBqBqB=B~B�B�BBB7B�B�B�BIBjBVBpB�B;B!�B!-B!-B vB vB$tB(�B(XB+�B.�B/�B0�B0�B1�B2�B2�B2�B4B5�B6�B72B9�B>]B@ B@ B@4BAUBCGBDMBF?BHKBJ=BK^BL�BQ�BUgBWYBW�BX+BZkB]�B^�B_�B`�B`�BbhBhsBl�Bm�Bo5Br-BtBu%Bv+BwLBy$Bz*B{dB}VB�B��B��B��B��B�^B�TB�sB�jB�ZB�2B�B�B�B�8B�8B�*B�DB�eB�CB�cB�}B��B��B��B��B��B��B��B��B��B�cB�%B�=B�0B�6B�"B�B�:B�[BյBخB��B��B�B�B��B��B��B��B��B��B��B��B��B�B�B�2B�8B�0B�qB�}B	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	OB	$@B	&fB	*eB	.cB	/OB	0UB	1[B	2aB	3hB	4�B	4nB	4nB	6zB	6zB	7�B	8�B	8�B	:�B	;�B	=�B	@�B	A�B	C�B	F�B	H1B	JXB	P}B	TaB	WYB	XyB	[qB	[WB	[WB	\]B	\]B	]dB	^jB	_pB	`vB	`�B	a�B	c�B	g�B	j�B	k�B	k�B	m�B	o�B	q�B	q�B	sB	u�B	xB	zB	{0B	{0B	|PB	cB	�OB	�;B	�AB	�'B	�GB	�GB	�{B	��B	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�B	�2B	�8B	�_B	�IB	��B	�vB	�|B	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�	B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�.B	�NB	�@B	�,B	�FB	�SB	�EB	�EB	�KB	�eB	�kB	�qB	ܒB	�jB	ބB	ߊB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	��B	��B	�B	��B	��B	�*B	��B	�B	�B	�BB	�.B	�.B
 4B
 4B
 iB
UB
[B
GB
gB
9B
9B
9B
SB
YB
_B
�B
�B
	�B
	lB
	lB

rB
xB
^B
xB
^B
^B
^B
xB
�B
�B
~B
dB
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
�B
!B
B
 'B
 B
 B
 B
!B
 �B
!�B
# B
$B
%B
%B
&B
'B
'8B
($B
($B
)*B
)DB
)*B
*0B
*0B
*0B
*KB
+6B
+QB
+QB
,WB
,WB
-CB
,WB
-CB
-CB
-]B
-wB
-]B
.IB
.cB
/OB
/iB
/iB
0UB
0oB
1[B
1[B
1[B
1vB
1[B
2GB
2aB
3�B
3hB
4nB
4nB
4nB
4nB
5�B
5tB
6�B
6�B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:�B
:�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
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
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
L�B
MB
L�B
MB
MB
MB
MB
N"B
N"B
OB
OB
OB
N�B
N�B
OB
OB
OB
P.B
PB
Q4B
QB
Q4B
Q B
Q B
Q B
Q B
QB
QB
RB
R B
RB
R B
R B
R:B
R B
R B
S@B
S[B
TaB
T,B
UB
U2B
U2B
UMB
V9B
V9B
VB
VB
V9B
VB
V9B
VSB
WYB
WYB
XEB
XEB
XEB
Y1B
X+B
XEB
YKB
YKB
YKB
ZQB
Z7B
ZkB
Z7B
ZQB
ZkB
ZQB
ZkB
[WB
[WB
[WB
\]B
\CB
\CB
\]B
\]B
]dB
]dB
]~B
^jB
^jB
^jB
^jB
_VB
_VB
_pB
_pB
_pB
_pB
`vB
`vB
`vB
`�B
`�B
`�B
a|B
a|B
abB
a|B
a|B
a|B
a�B
b�B
cnB
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
ezB
e�B
f�B
f�B
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
k�B
k�B
k�B
k�B
k�B
l�B
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
m�B
m�B
m�B
m�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9%~<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.25(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606180035542016061800355420160618003554201806221151402018062211514020180622115140201804050401442018040504014420180405040144  JA  ARFMdecpA19c                                                                20160624183516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094823  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094823  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094823  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094824  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094824  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094824  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094824  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160624094824  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160624094825                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102535                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160614153303  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160617153554  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160617153554  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190144  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622025140  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                