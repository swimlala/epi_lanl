CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:41Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ?A   AO  20111205113437  20190522121836  1901_5055_063                   2C  D   APEX                            2140                            040306                          846 @��ӆ`1   @�����@,�$�/��c��"��`1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A>ffA`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B���B�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�33B�  C   C�C  C  C  C
  C�fC�fC�fC�C  C  C�C  C�fC  C �C"  C#�fC%�fC'�fC*  C,  C.�C0�C2  C3�fC5�fC7�fC:  C<  C>�C@  CB  CD�CF  CH  CJ�CL  CN  CP  CR  CT  CV  CW�fCZ  C\�C^�C`  Cb  Cd  Cf  Ch  Ci�fCk�fCm�fCo�fCq�fCt  Cv  Cx  Cy�fC|  C~�C�  C�  C�  C�  C��3C��C�  C��3C�  C��C�  C��3C��C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C��C�  C��C�  C��3C�  C�  C�  C�  C��3C��C�  C�  C�  C��3C�  C�  C��3C��C��C�  C�  C�  C�  C�  C�  C��3C��C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C��C��3C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��C��3C�  C�  C��3C�  C�  C��3C�  C��C�  D   D �fD ��D� DfD� D��D� D  Dy�D  D�fD  D� DfD� D  D�fD	  D	y�D
  D
� D
��D� DfD� D  Dy�D  D�fD  Dy�D  D�fD  Dy�D  D� D��Dy�D  D� D  D� DfD� D  D�fD  Dy�D  D�fD  Dy�D  D�fD  Dy�D  D� D��D� DfD� D��D y�D!  D!�fD"fD"�fD#fD#�fD$  D$� D%  D%y�D&  D&�fD'fD'�fD(fD(� D)  D)y�D)��D*� D+fD+�fD,  D,� D,��D-y�D-��D.y�D.��D/y�D/��D0y�D1  D1� D2  D2� D3  D3�fD4  D4y�D4��D5y�D5��D6y�D7  D7� D8  D8�fD9fD9� D9�3D:y�D:��D;� D<  D<� D=  D=� D=��D>y�D>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM� DM�3DNs3DN��DOy�DO��DPy�DP��DQy�DR  DR�fDS  DSy�DS��DT� DU  DU� DV  DV�fDWfDW� DX  DXy�DX��DY� DZfDZ�fD[  D[� D\  D\� D]  D]� D^fD^�fD_fD_�fD`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Df��Dg� DhfDh� Di  Di�fDi��Dj� DkfDk� Dl  Dl�fDm  Dmy�Dn  Dn�fDo  Doy�Dp  Dp� Dp��Dq� Dr  D�FfD�y�D��fD��fD�� D�� D���D�#3D�&fD���D��3D�C3D�l�DӬ�D�� D�33D�3D��fD�VfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@�  @�  A  A6ffAX  Ax  A�  A���A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BM��BV  B^  Bf  Bn  Bv  B~  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B���B�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�33B�  B�  C��C� C� C� C	� CffCffCffC��C� C� C��C� CffC� C��C!� C#ffC%ffC'ffC)� C+� C-��C/��C1� C3ffC5ffC7ffC9� C;� C=��C?� CA� CC��CE� CG� CI��CK� CM� CO� CQ� CS� CU� CWffCY� C[��C]��C_� Ca� Cc� Ce� Cg� CiffCkffCmffCoffCqffCs� Cu� Cw� CyffC{� C}��C� C�� C�� C�� C��3C���C�� C��3C�� C���C�� C��3C���C�� C�� C�� C��3C�� C�� C�� C���C�� C�� C���C���C�� C���C�� C��3C�� C�� C�� C�� C��3C���C�� C�� C�� C��3C�� C�� C��3C���C���C�� C�� C�� C�� C�� C�� C��3C���C���C�� C�� C���C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cǳ3Cȳ3Cɳ3Cʳ3C�� C�� C�� C�� C�� C�� C���C���C�� CԳ3Cճ3C�� C�� C�� Cٳ3C�� C�� Cܳ3C�� C���C�� C�� C���C�3C�� C�� C�� C�3C�3C�� C�� C�� C�3C�� C�� C�� C�� C�� C���C�� C�� C���C��3C�� C�� C��3C�� C�� C��3C�� C���C�� C�� D ffD ٚD` D�fD` DٚD` D� DY�D� DffD� D` D�fD` D� DffD� D	Y�D	� D
` D
ٚD` D�fD` D� DY�D� DffD� DY�D� DffD� DY�D� D` DٚDY�D� D` D� D` D�fD` D� DffD� DY�D� DffD� DY�D� DffD� DY�D� D` DٚD` D�fD` DٚD Y�D � D!ffD!�fD"ffD"�fD#ffD#� D$` D$� D%Y�D%� D&ffD&�fD'ffD'�fD(` D(� D)Y�D)ٚD*` D*�fD+ffD+� D,` D,ٚD-Y�D-ٚD.Y�D.ٚD/Y�D/ٚD0Y�D0� D1` D1� D2` D2� D3ffD3� D4Y�D4ٚD5Y�D5ٚD6Y�D6� D7` D7� D8ffD8�fD9` D9�3D:Y�D:ٚD;` D;� D<` D<� D=` D=ٚD>Y�D>ٚD?Y�D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDM` DM�3DNS3DNٚDOY�DOٚDPY�DPٚDQY�DQ� DRffDR� DSY�DSٚDT` DT� DU` DU� DVffDV�fDW` DW� DXY�DXٚDY` DY�fDZffDZ� D[` D[� D\` D\� D]` D]�fD^ffD^�fD_ffD_�fD`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� DfY�DfٚDg` Dg�fDh` Dh� DiffDiٚDj` Dj�fDk` Dk� DlffDl� DmY�Dm� DnffDn� DoY�Do� Dp` DpٚDq` Dq� D�6fD�i�D��fD��fD�p D�p D��D�3D�fD���D��3D�33D�\�DӜ�D�� D�#3D�s3D��fD�FfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AƁAƋDAƍPAƋDAƉ7AƏ\AƋDAƕ�AƏ\AƏ\AƏ\AƑhAƑhAƕ�Aƙ�AƝ�AƝ�Aơ�Aơ�Aơ�Aƣ�Aƥ�Aƣ�Aƥ�Aƥ�AƧ�AƬAƧ�Aƛ�Aƙ�AƓuA�bNA��Ać+A§�A��PA��A�jA�E�A���A��9A�x�A�G�A��wA�ffA�  A��DA�K�A�  A�K�A�ĜA�  A���A�A�hsA��FA� �A��DA�bNA��jA�O�A��;A���A��jA��A�A�33A��^A�%A��wA� �A��FA��A�z�A��A��uA���A�JA�ZA�A��A��-A�A�v�A��HA�/A���A���A��+A�ZA���A�A�~�A���A�1A�G�A�Q�A}�;A{�Az9XAt�yAo
=Aj  Ae��Aa�
A^��A[l�AW�wAU��ATffAPAM�AL�`AKt�AH�AD�ACG�AA��A?oA<��A;ƨA9��A7A3K�A0�A0�A/�mA/G�A-�PA+�A(bA'\)A&�A&�uA&JA%hsA#7LA"��A ��A!+A!�FA!�A!�;A#A#�
A%�A%�TA&jA&�uA%\)A$��A$~�A#�mA#�A#7LA"�9A"1'A"bA"�A"M�A!�TA!�A ��A!A �A ��A $�A��A�`AE�A�AbNA�PA/A�`Av�AI�AJA�TA��A�PAp�AdZA\)A;dAVA��A�A��At�A��A�A�A�!A�wA|�A?}AC�AO�A|�Al�AVA��A��A�AA�A1AC�A�AbAt�A��A�!A��A�\AJA
��A
5?A	�A	��A	p�A	`BA	`BA	x�A	|�A	S�A��A�+A�A`BA
=A^5A��A�AhsA\)AoAr�AI�A�A��AȴAE�A�A��A�hAC�A �HA ��@��;@�V@�@��`@�1'@��
@��H@��@�G�@�j@��w@�33@�^5@�E�@���@�&�@�bN@�C�@���@�h@���@��@�1'@��@�"�@��@�ȴ@��^@�Z@�w@�t�@�l�@��H@�ff@���@��@�Z@�w@�t�@�+@��y@��@��@�M�@�x�@�1'@�33@�R@�V@�@�O�@�@ߍP@ޏ\@��#@�G�@��
@�
=@ڸR@�{@�Ĝ@��m@�33@��y@���@�5?@�z�@�l�@�@��@��/@�1@ϥ�@�l�@�;d@��y@·+@ͩ�@��@̛�@�+@�E�@Ɂ@�/@�Z@�\)@Ɨ�@�5?@ŉ7@�G�@Ł@őh@�X@�?}@�/@�&�@���@�z�@��@�
=@��@��^@��@�G�@��`@���@�r�@�ƨ@��R@�v�@��@��-@�G�@���@��D@�Z@�b@��;@��F@��@���@��T@�hs@��@��`@��D@�Q�@�I�@�9X@��m@�@�=q@�@���@��h@�O�@�7L@�V@���@�z�@�1@��;@��P@�"�@���@�5?@��h@�/@���@���@��@��u@��@�Q�@� �@��@��w@��@��P@�ȴ@�J@�O�@���@�A�@�C�@�+@��y@�5?@�@��@��^@�V@���@�Z@���@�"�@��@���@�M�@��@�p�@���@��/@���@��@��D@�r�@�Z@�9X@���@�l�@�;d@�o@��y@���@��+@�-@�J@��#@��7@�/@���@���@��@�9X@��;@���@��@�dZ@�+@�o@���@��@��@���@��\@�J@�X@��j@�bN@�b@��w@���@�t�@�K�@�"�@���@�~�@�M�@��@�@��#@�hs@���@���@�$�@x��@p�u@e�h@Z^5@Pr�@Gl�@=��@6��@/�;@+S�@%�T@ 1'@��@Q�@��@K�@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AƁAƋDAƍPAƋDAƉ7AƏ\AƋDAƕ�AƏ\AƏ\AƏ\AƑhAƑhAƕ�Aƙ�AƝ�AƝ�Aơ�Aơ�Aơ�Aƣ�Aƥ�Aƣ�Aƥ�Aƥ�AƧ�AƬAƧ�Aƛ�Aƙ�AƓuA�bNA��Ać+A§�A��PA��A�jA�E�A���A��9A�x�A�G�A��wA�ffA�  A��DA�K�A�  A�K�A�ĜA�  A���A�A�hsA��FA� �A��DA�bNA��jA�O�A��;A���A��jA��A�A�33A��^A�%A��wA� �A��FA��A�z�A��A��uA���A�JA�ZA�A��A��-A�A�v�A��HA�/A���A���A��+A�ZA���A�A�~�A���A�1A�G�A�Q�A}�;A{�Az9XAt�yAo
=Aj  Ae��Aa�
A^��A[l�AW�wAU��ATffAPAM�AL�`AKt�AH�AD�ACG�AA��A?oA<��A;ƨA9��A7A3K�A0�A0�A/�mA/G�A-�PA+�A(bA'\)A&�A&�uA&JA%hsA#7LA"��A ��A!+A!�FA!�A!�;A#A#�
A%�A%�TA&jA&�uA%\)A$��A$~�A#�mA#�A#7LA"�9A"1'A"bA"�A"M�A!�TA!�A ��A!A �A ��A $�A��A�`AE�A�AbNA�PA/A�`Av�AI�AJA�TA��A�PAp�AdZA\)A;dAVA��A�A��At�A��A�A�A�!A�wA|�A?}AC�AO�A|�Al�AVA��A��A�AA�A1AC�A�AbAt�A��A�!A��A�\AJA
��A
5?A	�A	��A	p�A	`BA	`BA	x�A	|�A	S�A��A�+A�A`BA
=A^5A��A�AhsA\)AoAr�AI�A�A��AȴAE�A�A��A�hAC�A �HA ��@��;@�V@�@��`@�1'@��
@��H@��@�G�@�j@��w@�33@�^5@�E�@���@�&�@�bN@�C�@���@�h@���@��@�1'@��@�"�@��@�ȴ@��^@�Z@�w@�t�@�l�@��H@�ff@���@��@�Z@�w@�t�@�+@��y@��@��@�M�@�x�@�1'@�33@�R@�V@�@�O�@�@ߍP@ޏ\@��#@�G�@��
@�
=@ڸR@�{@�Ĝ@��m@�33@��y@���@�5?@�z�@�l�@�@��@��/@�1@ϥ�@�l�@�;d@��y@·+@ͩ�@��@̛�@�+@�E�@Ɂ@�/@�Z@�\)@Ɨ�@�5?@ŉ7@�G�@Ł@őh@�X@�?}@�/@�&�@���@�z�@��@�
=@��@��^@��@�G�@��`@���@�r�@�ƨ@��R@�v�@��@��-@�G�@���@��D@�Z@�b@��;@��F@��@���@��T@�hs@��@��`@��D@�Q�@�I�@�9X@��m@�@�=q@�@���@��h@�O�@�7L@�V@���@�z�@�1@��;@��P@�"�@���@�5?@��h@�/@���@���@��@��u@��@�Q�@� �@��@��w@��@��P@�ȴ@�J@�O�@���@�A�@�C�@�+@��y@�5?@�@��@��^@�V@���@�Z@���@�"�@��@���@�M�@��@�p�@���@��/@���@��@��D@�r�@�Z@�9X@���@�l�@�;d@�o@��y@���@��+@�-@�J@��#@��7@�/@���@���@��@�9X@��;@���@��@�dZ@�+@�o@���@��@��@���@��\@�J@�X@��j@�bN@�b@��w@���@�t�@�K�@�"�@���@�~�@�M�@��@�@��#@�hs@���@���@�$�@x��@p�u@e�h@Z^5@Pr�@Gl�@=��@6��@/�;@+S�@%�T@ 1'@��@Q�@��@K�@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	ȴB
A�B
�}BVB�B=qBS�BcTBdZB�7B�-B��B�B�TB�B�B��B��BBuB�BDB��B��BȴB�LB��B��B�XBǮBŢB��B��B�)B�BB�5B�;B�mB�TB�;B�)B�#B�B�
B��B��B��B��B��BƨB�jB��B��B�hB�PB� Bn�BXB9XBPB
�/B
�B
jB
<jB
�B	��B	�/B	��B	�B	� B	[#B	@�B	%�B	oB��B�B�HB�
BɺBÖB�wB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�XBB��B��B��B�B�B�B��B	B	bB	�B	A�B	]/B	�B	��B	��B	�dB	�qB	ȴB	��B	��B	��B	��B	�
B	�/B	�/B	�fB	��B	��B	�B	�B	��B	��B	��B	��B
B
	7B
	7B
1B

=B
	7B

=B
\B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
#�B
$�B
%�B
%�B
'�B
'�B
$�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
oB
hB
hB
bB
bB
\B
\B
\B
VB
\B
hB
oB
hB
bB
bB
bB
bB
bB
\B
VB
PB
PB
JB
PB
JB
DB

=B

=B
	7B
	7B
	7B
	7B
1B
1B
+B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�sB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
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
DB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
PB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
�B
$�B
(�B
0!B
7LB
?}B
H�B
M�B
R�B
[#B
^5B
dZB
gmB
jB
o�B
s�B
v�B
z�B
}�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	��B
N�B
ȴBuB#�B?}BW
BffBffB�JB�LB��B�5B�sB�B��BJBhB�B2-B1'B�B%B�B�BÖB�B�B�}B��B��B�
B�)B�`B�mB�yB�B�B�yB�mB�ZB�NB�;B�HB�5B�B�B�B�B��B��B�?B��B��B��B�oB�Bp�B\)B0!BB
�B
�VB
W
B
49B
VB	�B	��B	��B	��B	x�B	ZB	>wB	,B	�B��B�B�B�B��B��B��B��B�^B�dB�dB�LB�-B�?B�?B�RB�B��B��B��B��B��B�-B�FB�qBǮB��B�
B�HB�;B��B�yB��B	B	JB	hB	:^B	S�B	� B	�uB	��B	ÖB	��B	��B	��B	��B	��B	��B	�B	�5B	�/B	�fB	��B	��B	��B	�B	��B	��B
  B
B
DB
\B
\B
oB
bB
JB
PB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
%�B
&�B
"�B
�B
 �B
�B
 �B
"�B
�B
�B
�B
�B
�B
�B
�B
#�B
"�B
%�B
'�B
,B
+B
33B
.B
(�B
+B
&�B
$�B
!�B
 �B
!�B
$�B
%�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
oB
uB
uB
�B
�B
{B
�B
{B
uB
bB
bB
uB
bB
oB
VB
bB
\B
JB
hB
JB
bB
\B

=B
JB
DB
	7B
JB
1B

=B

=B
%B
DB
%B
+B
%B
B
+B
B
%B
B
B
B
B
B
B
B	��B	��B
B
B	��B	��B	��B	��B	��B	��B
B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�B	�B	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B
  B
B
B
B
B
B
B
%B
B
+B
B
B
1B
1B
	7B
1B
+B
1B

=B
DB

=B

=B
DB

=B

=B
DB
DB
JB
DB
DB
JB
DB
DB
DB
DB
DB
PB
PB
PB
PB
PB
VB
VB
\B
VB
VB
VB
\B
VB
VB
\B
VB
\B
bB
PB
VB
oB
bB
oB
hB
hB
oB
hB
oB
hB
uB
oB
uB
uB
oB
{B
uB
�B
$�B
(�B
0!B
7LB
?}B
H�B
N�B
S�B
[#B
^5B
dZB
gmB
jB
p�B
t�B
v�B
z�B
}�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<��
<���<�h<�<�1<�t�<�C�<#�
<�C�<D��<#�
<#�
<#�
<#�
<49X<49X<49X<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<�t�<�o<#�
<#�
<T��<�C�<�t�<ě�=C�=C�=��=,1=\)<���<�/<�t�<u=t�=�w=o<�h<���<�j<���<�j<e`B<�o<�/<u<#�
<u<�9X<�/<#�
<u<�t�<u<D��<�t�<��
<�<u<#�
<#�
<#�
<u<���<���<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250292012011312502920120113125029  AO  ARGQ                                                                        20111205113437  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113437  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125029  IP                  G�O�G�O�G�O�                