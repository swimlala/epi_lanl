CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:58Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               HA   AO  20111130144237  20190522121829  1728_5048_072                   2C  D   APEX                            2142                            040306                          846 @�ݭ���1   @�ݯ6� @6��C���b���+1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy� D��3D�@ D�` D�� D�	�D�,�D���D��fD��3D��D��fD�� D�� D�FfD�p D� D���D� D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A���A�  B  B  B  B  B&  B.  B5��B>  BF  BN  BV  B^  Bf  Bn  Bv  B~  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C��C� C	� C� C� C� C� C� C� C��C��C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� DY�D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(ffD(�fD)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` DiٚDj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� DwL�Dy` D��3D�0 D�P D�� D���D��D���D��fD��3D�	�D��fDǰ D�� D�6fD�` D� D���D�  D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�{A�%A�z�A��A�33A���AȺ^Aȕ�Aț�Aȇ+Aȉ7AȃAȁA�n�A�ZA�C�A�/A��A�JA��`A�\)A�z�A���A�Q�A��
A���A�p�A�Q�A��PA�&�A�l�A�r�A�{A��A��RA���A�t�A��#A��HA�O�A��A��HA�~�A�?}A�`BA��A��A�l�A�%A��+A���A��-A�\)A���A�A��A��wA��A�ȴA���A��A�r�A��DA�  A���A��A�bNA��yA�\)A�
=A��-A��A�&�A��A�t�A�~�A��#A���A���A��A���A�l�A���A�`BA� �A��9A��hA�;dA��yA���A�%A���A��A��A��yA�t�A��A���A���A���A���A�"�A�bNA�1'A���A�VA��jA�v�A�Q�A~�A|��Azr�Awp�Au��AtA�Ar��Aq�Ap$�AnĜAl�yAj^5Ah�Af��Ad-AaS�A^JAZȴAX�/AWC�AR��AN�AL�+AK��AI��AF��AE�AE?}AD�ACXAB�9AB1AA\)A@E�A>A�A<��A<��A<�A;�A:A9��A9&�A8�`A8��A8��A8~�A7��A6��A5��A4I�A3%A2E�A1x�A1�A0v�A/x�A/
=A.��A.5?A-��A-A*��A)��A)%A(I�A'��A%��A$1A#C�A"�9A"ffA!��A!�A ��A -A7LAM�A��AE�A�A�9AhsA1'AA�AVA�^A��A5?AO�AbNAXA�jA|�A�A/A$�At�A
�`A
JA	K�A�!AjA  A+A��A��AbA��A��A�^A Z@�~�@�x�@�K�@�`B@��/@�  @��+@�O�@�v�@�7L@�z�@�v�@��@��@�-@���@�;d@��@�ff@�n�@�-@�?}@��@�ff@ߍP@݁@�@��@�  @�
=@�n�@���@���@�O�@���@�Z@��
@�dZ@Ώ\@͡�@�Z@˝�@�C�@�33@��H@�^5@�-@��@�G�@ǝ�@�$�@���@�ƨ@Å@�\)@�@¸R@�~�@���@�p�@�?}@���@���@�(�@�+@�ȴ@�@��@��D@��@���@��y@���@���@� �@�|�@�33@���@�~�@�@�@��h@���@��7@��@�Q�@�K�@�+@�C�@�;d@�33@�+@�@���@�M�@�x�@���@��@��u@��u@���@���@���@�I�@�@�(�@���@�ff@���@���@���@�\)@��y@��R@�ff@�M�@�V@�^5@�J@��-@�@���@��h@��@�1@�|�@��y@�n�@�{@���@��@�hs@�?}@��@��`@�z�@�1@���@�K�@��@���@��R@�ȴ@�V@��-@�O�@�?}@��@���@���@�Ĝ@�(�@� �@� �@�b@��;@��F@���@��P@��@�l�@�S�@�33@�o@���@��@��@�M�@���@��h@�x�@�O�@�V@���@��9@���@�A�@�1@���@�1@��;@��;@�Z@�1'@���@��@��F@��F@���@�dZ@�33@�
=@��H@��\@�E�@�{@��@���@��-@�p�@��@��/@���@�A�@��;@��F@���@�l�@�C�@��@���@��\@��@��#@�@���@�x�@�X@�&�@��9@�z�@�Z@�1'@��w@�|�@�K�@�+@�@��@�ȴ@��+@�V@�=q@�@��@�@��7@�&�@���@��`@���@���@�9X@�9X@�1@��F@���@�t�@�\)@�S�@�;d@��@���@���@��\@�ff@�E�@�$�@�J@��@�@��7@�hs@��@��`@���@��D@�Q�@�(�@�b@�ƨ@��P@��H@~V@s�@kC�@c��@\Z@T��@N�y@Gl�@@b@9��@5��@.�R@*�!@%O�@��@��@��@Z@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�{A�%A�z�A��A�33A���AȺ^Aȕ�Aț�Aȇ+Aȉ7AȃAȁA�n�A�ZA�C�A�/A��A�JA��`A�\)A�z�A���A�Q�A��
A���A�p�A�Q�A��PA�&�A�l�A�r�A�{A��A��RA���A�t�A��#A��HA�O�A��A��HA�~�A�?}A�`BA��A��A�l�A�%A��+A���A��-A�\)A���A�A��A��wA��A�ȴA���A��A�r�A��DA�  A���A��A�bNA��yA�\)A�
=A��-A��A�&�A��A�t�A�~�A��#A���A���A��A���A�l�A���A�`BA� �A��9A��hA�;dA��yA���A�%A���A��A��A��yA�t�A��A���A���A���A���A�"�A�bNA�1'A���A�VA��jA�v�A�Q�A~�A|��Azr�Awp�Au��AtA�Ar��Aq�Ap$�AnĜAl�yAj^5Ah�Af��Ad-AaS�A^JAZȴAX�/AWC�AR��AN�AL�+AK��AI��AF��AE�AE?}AD�ACXAB�9AB1AA\)A@E�A>A�A<��A<��A<�A;�A:A9��A9&�A8�`A8��A8��A8~�A7��A6��A5��A4I�A3%A2E�A1x�A1�A0v�A/x�A/
=A.��A.5?A-��A-A*��A)��A)%A(I�A'��A%��A$1A#C�A"�9A"ffA!��A!�A ��A -A7LAM�A��AE�A�A�9AhsA1'AA�AVA�^A��A5?AO�AbNAXA�jA|�A�A/A$�At�A
�`A
JA	K�A�!AjA  A+A��A��AbA��A��A�^A Z@�~�@�x�@�K�@�`B@��/@�  @��+@�O�@�v�@�7L@�z�@�v�@��@��@�-@���@�;d@��@�ff@�n�@�-@�?}@��@�ff@ߍP@݁@�@��@�  @�
=@�n�@���@���@�O�@���@�Z@��
@�dZ@Ώ\@͡�@�Z@˝�@�C�@�33@��H@�^5@�-@��@�G�@ǝ�@�$�@���@�ƨ@Å@�\)@�@¸R@�~�@���@�p�@�?}@���@���@�(�@�+@�ȴ@�@��@��D@��@���@��y@���@���@� �@�|�@�33@���@�~�@�@�@��h@���@��7@��@�Q�@�K�@�+@�C�@�;d@�33@�+@�@���@�M�@�x�@���@��@��u@��u@���@���@���@�I�@�@�(�@���@�ff@���@���@���@�\)@��y@��R@�ff@�M�@�V@�^5@�J@��-@�@���@��h@��@�1@�|�@��y@�n�@�{@���@��@�hs@�?}@��@��`@�z�@�1@���@�K�@��@���@��R@�ȴ@�V@��-@�O�@�?}@��@���@���@�Ĝ@�(�@� �@� �@�b@��;@��F@���@��P@��@�l�@�S�@�33@�o@���@��@��@�M�@���@��h@�x�@�O�@�V@���@��9@���@�A�@�1@���@�1@��;@��;@�Z@�1'@���@��@��F@��F@���@�dZ@�33@�
=@��H@��\@�E�@�{@��@���@��-@�p�@��@��/@���@�A�@��;@��F@���@�l�@�C�@��@���@��\@��@��#@�@���@�x�@�X@�&�@��9@�z�@�Z@�1'@��w@�|�@�K�@�+@�@��@�ȴ@��+@�V@�=q@�@��@�@��7@�&�@���@��`@���@���@�9X@�9X@�1@��F@���@�t�@�\)@�S�@�;d@��@���@���@��\@�ff@�E�@�$�@�J@��@�@��7@�hs@��@��`@���@��D@�Q�@�(�@�b@�ƨ@��P@��H@~V@s�@kC�@c��@\Z@T��@N�y@Gl�@@b@9��@5��@.�R@*�!@%O�@��@��@��@Z@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�PB�PB�PB��BĜB�?B�-B�?B�?B�qB�}BƨBǮBȴBǮBŢBŢBÖBBBÖB��BDB
=BJBPBVBVBuB�B�B�B�B�B �B �B �B �B!�B"�B �B�B�B�B�B�B�B�BuB{BVB+B%BBB��B��B��B��B��B�B�B�;B��BȴBĜBÖB��B��B��BĜB��B�dB�bBl�BO�B=qB#�B\BbBbBhBBPB7LBF�B2-B�B�B�B��B��B��B�bB|�Be`B_;BP�B<jB0!B%�B�BuB	7B
�B
ĜB
v�B
R�B
7LB
%�B
�B
JB	��B	�ZB	��B	��B	��B	�3B	�B	��B	�oB	�%B	� B	ffB	O�B	<jB	1'B	)�B	 �B	%B�`B��B��BŢB�dB�3B�FB�LB�?B�-B�B�B��B��B��B�uB�bB�\B�oB��B��B��B��B��B��B��B��B�uB�\B�JB�7B�1B�B�B�%B�7B� B~�B~�B� Bx�Bs�Br�Bo�Bs�Bs�Bn�Bk�BjBgmBffBe`BffBgmBffBl�Be`BaHBaHBaHB^5B^5B_;B^5B_;B]/B_;B_;B_;BaHB`BB`BB_;B`BB^5BdZB`BB`BBbNBbNB^5B]/B\)BZB[#BXBR�BR�BW
B]/BZBO�BW
BO�BW
BI�BK�BQ�BXBI�BC�BF�B@�BG�BC�B@�BC�B@�BE�BC�BI�BO�BM�BN�BN�BL�BF�B?}B<jB?}BD�BA�B@�B>wB>wB@�BD�BI�BK�BM�BQ�BVB\)B\)BaHBgmBo�Bt�Bt�Bu�Bv�Bv�Bv�By�B|�B|�B}�B�B�B�1B�PB�\B�bB�{B��B��B��B��B��B��B��B��B��B�B�B�9B�FB�XB�dB��BƨB��B��B�B�;B�;B�TB�ZB�mB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�yB�B�yB�B�B�B�B�B�B��B��B��B��B	B	DB	�B	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	)�B	+B	-B	.B	/B	0!B	2-B	8RB	=qB	?}B	B�B	C�B	D�B	D�B	E�B	H�B	J�B	N�B	Q�B	T�B	VB	W
B	ZB	[#B	^5B	`BB	`BB	aHB	cTB	e`B	hsB	k�B	m�B	o�B	s�B	v�B	w�B	y�B	|�B	� B	�B	�B	�%B	�1B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�FB	�LB	�FB	�^B	�qB	�}B	�wB	�}B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
  B
DB
�B
!�B
'�B
/B
49B
<jB
A�B
G�B
L�B
S�B
YB
^5B
cTB
ffB
hsB
n�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�PB�PB�\B��BȴB�LB�-B�FB�?B�wB�}BƨBǮBȴBǮBŢBŢBÖBBÖBŢB�B�BhBVBVB\B{B�B�B�B�B �B"�B"�B!�B!�B#�B%�B$�B#�B�B�B�B �B�B �B�B�B�BoBPB1B+B1B  B��B��B��B��B��B�B�ZB��B��BŢBĜBĜBÖBBƨB��BŢB��Br�BVBF�B(�BbBhBoBuBBJB8RBL�B7LB�B�B�B��B��B�B��B�BhsBcTBW
BB�B5?B,B�B�BbB
��B
�B
�B
ZB
>wB
+B
!�B
uB
B	�B	�B	��B	ŢB	�FB	�-B	�B	��B	�7B	�B	l�B	W
B	D�B	9XB	/B	%�B	hB�B�B��B��BÖB�?B�RB�dB�RB�9B�'B�!B�B��B��B�{B�hB�oB��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�DB�+B�+B�7B�=B�B� B� B�B}�Bv�Bu�Bq�Bu�Bx�Bs�Bm�Bl�BhsBhsBgmBhsBiyBiyBo�BgmBe`Be`BbNBbNBbNBaHB`BBbNB_;BbNBbNBbNBdZBdZBbNBcTBe`BaHBhsBcTBbNBe`Be`B`BB^5B^5B^5BaHB\)BT�BT�B[#BaHB^5BR�BYBR�BZBJ�BM�BS�BZBM�BE�BH�BC�BI�BE�BC�BE�BC�BG�BC�BI�BQ�BN�BP�BQ�BQ�BJ�BE�B>wBA�BF�BC�BD�BB�B?}BA�BE�BJ�BL�BO�BS�BYB^5B]/BbNBhsBp�Bu�Bu�Bu�By�By�By�B{�B}�B}�B~�B�B�B�7B�VB�bB�hB��B��B��B��B��B��B��B��B��B��B�B�!B�?B�LB�^B�jBBǮB��B��B�B�BB�BB�`B�fB�sB�B�B�B��B��B��B��B��B��B��B��B��B	  B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	PB	�B	{B	{B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	%�B	+B	,B	.B	/B	/B	0!B	33B	9XB	>wB	?}B	C�B	D�B	E�B	D�B	F�B	H�B	J�B	N�B	R�B	VB	VB	W
B	ZB	\)B	_;B	aHB	aHB	aHB	cTB	ffB	iyB	l�B	m�B	p�B	t�B	w�B	x�B	y�B	}�B	�B	�B	�B	�%B	�7B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�!B	�-B	�-B	�3B	�LB	�RB	�LB	�dB	�wB	��B	�}B	��B	��B	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
  B
DB
�B
!�B
'�B
/B
5?B
=qB
A�B
G�B
L�B
S�B
YB
^5B
cTB
ffB
hsB
n�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<���<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452182012011014521820120110145218  AO  ARGQ                                                                        20111130144237  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144237  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145218  IP                  G�O�G�O�G�O�                