CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:57Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               CA   AO  20111130144204  20190522121829  1728_5048_067                   2C  D   APEX                            2142                            040306                          846 @��CA�1   @��C����@6K��Q��c	�E��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCk�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvl�DyffD�  D�@ D�|�D�� D��3D�&fD�S3D��fD��fD�,�D�S3Dǳ3D�� D�&fDڌ�D�fD�� D�fD�L�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@���@�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B}��B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� CiffCkffCm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` DٚDY�D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� DY�D� D` D� D` D� D` D� D` D�fD` D� D` D� D` D� D` D� D` D� D` D� DY�D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"ٚD#Y�D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DAٚDB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� DvL�DyFfD�� D�0 D�l�D�� D��3D�fD�C3D��fD��fD��D�C3Dǣ3D�� D�fD�|�D�fD�� D�fD�<�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��hA�?}A��\A�l�A�`BA�Q�A�I�A�C�A�;dA�1'A�/A�/A�1'A�1'A�-A�&�A��A�{A�bA�JA�JA�JA�JA�VA�A��HA��A���A���A�(�A��A���A�v�A�(�A��TA�M�A���A�ĜA���A�z�A��yA��HA���A��A��A���A�ƨA���A��A�^5A�$�A���A��;A���A��A�p�A�(�A���A��A�/A���A��7A�p�A�VA�$�A��FA�|�A��A���A�v�A�&�A��7A��jA�G�A��TA��\A��A�O�A��/A���A��yA�A�x�A���A�^5A�1'A�A�ƨA��
A�-A�ȴA���A��FA��DA���A�ffA��hA�&�A��A�XA���A�ffA�1'A��RA�9XA��FA��A��`A���A���A��7A�C�A���A�;dA��mA�ĜA�ƨA��`A�"�A�z�A�;dA���A�oA�z�A�  A�7LA��A�r�A� �A�I�A��#A�G�A���A�7LA��A�VA��RA�E�A}7LAt��Aq
=Am��Ak�7Aj�`Ag|�Ad��Ad�A`r�A]�A[�AY�AW��AU"�AS��AP��AL�HAK%AI/AG��AE�
AD�yADĜAD��ADĜADv�AB�RAAdZA@jA>(�A<��A;`BA:  A8��A7�A65?A5l�A4~�A3;dA2I�A0~�A0=qA0=qA0Q�A/�
A/��A.�/A-�A,��A+�-A*9XA(��A'x�A%�;A%O�A$ �A"-A!��A!�A 1AK�A��A�AJAA�AAA�Al�A(�A+AƨA�yA��A�A�A%A\)A�jA~�A{Ap�A
��A
Q�A	��A
  A	��A	�wA	"�A1A�HA�A&�A�DAx�A�#A
=@��@�@�dZ@��T@�O�@��j@�P@�x�@�(�@�!@�/@�33@�x�@�(�@�l�@�@�~�@�+@�5?@�G�@��@�33@�V@�t�@��@�bN@�I�@�|�@��@�O�@�  @�E�@�O�@�9X@ӥ�@ӕ�@�K�@�M�@�%@�Z@�\)@��@�V@͙�@�&�@��/@̓u@�1'@��m@�K�@�o@�~�@���@ư!@�Q�@Õ�@§�@�/@��@��@��P@�-@�hs@��`@�I�@��;@�S�@���@�@� �@�+@���@��/@��@��+@��^@�7L@��j@�(�@���@�"�@�^5@��@��^@��7@�X@�G�@�X@�O�@���@�b@���@�\)@�33@��H@�M�@���@��@��9@��@���@��u@�I�@� �@� �@���@�1@�Ĝ@�O�@�`B@�G�@�7L@�?}@��@��@���@�z�@� �@��@���@�o@�$�@��-@���@�`B@�Ĝ@�z�@�1'@�  @��@�dZ@�C�@�
=@��!@�ff@�=q@�@��@�`B@��@�O�@��@��@��@�%@�j@�  @��w@���@�33@��#@��7@�O�@��@�Ĝ@��u@��u@���@���@�j@���@�l�@�;d@�
=@���@�E�@���@���@���@��!@�J@�@�`B@�Q�@�j@��/@�O�@�&�@�7L@��7@���@���@��h@�/@��@�A�@��@��m@�t�@���@�5?@���@�@��h@�hs@�7L@���@��9@�I�@��@���@��F@��@�l�@�C�@��@���@�^5@�E�@�5?@�-@�@��#@��-@���@���@��7@�G�@�/@��@��@���@��@�Ĝ@��u@�1'@��@��P@�t�@�33@�o@��y@���@�ff@�E�@��@���@�/@��@��j@��D@�bN@�1'@��@��w@��@�S�@��@��@���@�v�@�V@�M�@�=q@�@��^@���@��D@\)@t��@j��@bn�@^5?@W|�@PA�@JJ@Ct�@:~�@4�@/l�@*�!@&��@ �9@ƨ@&�@$�@��@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��hA�?}A��\A�l�A�`BA�Q�A�I�A�C�A�;dA�1'A�/A�/A�1'A�1'A�-A�&�A��A�{A�bA�JA�JA�JA�JA�VA�A��HA��A���A���A�(�A��A���A�v�A�(�A��TA�M�A���A�ĜA���A�z�A��yA��HA���A��A��A���A�ƨA���A��A�^5A�$�A���A��;A���A��A�p�A�(�A���A��A�/A���A��7A�p�A�VA�$�A��FA�|�A��A���A�v�A�&�A��7A��jA�G�A��TA��\A��A�O�A��/A���A��yA�A�x�A���A�^5A�1'A�A�ƨA��
A�-A�ȴA���A��FA��DA���A�ffA��hA�&�A��A�XA���A�ffA�1'A��RA�9XA��FA��A��`A���A���A��7A�C�A���A�;dA��mA�ĜA�ƨA��`A�"�A�z�A�;dA���A�oA�z�A�  A�7LA��A�r�A� �A�I�A��#A�G�A���A�7LA��A�VA��RA�E�A}7LAt��Aq
=Am��Ak�7Aj�`Ag|�Ad��Ad�A`r�A]�A[�AY�AW��AU"�AS��AP��AL�HAK%AI/AG��AE�
AD�yADĜAD��ADĜADv�AB�RAAdZA@jA>(�A<��A;`BA:  A8��A7�A65?A5l�A4~�A3;dA2I�A0~�A0=qA0=qA0Q�A/�
A/��A.�/A-�A,��A+�-A*9XA(��A'x�A%�;A%O�A$ �A"-A!��A!�A 1AK�A��A�AJAA�AAA�Al�A(�A+AƨA�yA��A�A�A%A\)A�jA~�A{Ap�A
��A
Q�A	��A
  A	��A	�wA	"�A1A�HA�A&�A�DAx�A�#A
=@��@�@�dZ@��T@�O�@��j@�P@�x�@�(�@�!@�/@�33@�x�@�(�@�l�@�@�~�@�+@�5?@�G�@��@�33@�V@�t�@��@�bN@�I�@�|�@��@�O�@�  @�E�@�O�@�9X@ӥ�@ӕ�@�K�@�M�@�%@�Z@�\)@��@�V@͙�@�&�@��/@̓u@�1'@��m@�K�@�o@�~�@���@ư!@�Q�@Õ�@§�@�/@��@��@��P@�-@�hs@��`@�I�@��;@�S�@���@�@� �@�+@���@��/@��@��+@��^@�7L@��j@�(�@���@�"�@�^5@��@��^@��7@�X@�G�@�X@�O�@���@�b@���@�\)@�33@��H@�M�@���@��@��9@��@���@��u@�I�@� �@� �@���@�1@�Ĝ@�O�@�`B@�G�@�7L@�?}@��@��@���@�z�@� �@��@���@�o@�$�@��-@���@�`B@�Ĝ@�z�@�1'@�  @��@�dZ@�C�@�
=@��!@�ff@�=q@�@��@�`B@��@�O�@��@��@��@�%@�j@�  @��w@���@�33@��#@��7@�O�@��@�Ĝ@��u@��u@���@���@�j@���@�l�@�;d@�
=@���@�E�@���@���@���@��!@�J@�@�`B@�Q�@�j@��/@�O�@�&�@�7L@��7@���@���@��h@�/@��@�A�@��@��m@�t�@���@�5?@���@�@��h@�hs@�7L@���@��9@�I�@��@���@��F@��@�l�@�C�@��@���@�^5@�E�@�5?@�-@�@��#@��-@���@���@��7@�G�@�/@��@��@���@��@�Ĝ@��u@�1'@��@��P@�t�@�33@�o@��y@���@�ff@�E�@��@���@�/@��@��j@��D@�bN@�1'@��@��w@��@�S�@��@��@���@�v�@�V@�M�@�=q@�@��^@���@��D@\)@t��@j��@bn�@^5?@W|�@PA�@JJ@Ct�@:~�@4�@/l�@*�!@&��@ �9@ƨ@&�@$�@��@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�LB�FB�FB�RB�XB�dB�jB�wB�wB�wB�}B��BÖBŢBƨBǮBǮBǮBȴBɺBɺB��B��B�B��B  BB%BoB;dBL�BYBbNBdZBk�Bs�Br�Bv�B�B�B}�B�B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�RB�wB�XB�FB��B�{B��B��B�PB�%B�B�VB�7B{�BhsBD�BcTBr�Br�BK�B	7B��B��BBB��B�B�B�B��BĜB�yB%BPBhB{B{B{B\B+B��B��B�B�BB�B�B�B�
B��B��BȴB�qB�B��B�JB�Bw�Bq�Bk�BaHBQ�BI�BA�B�B
�B
ŢB
�3B
��B
��B
��B
�PB
�JB
�JB
bNB
cTB
%B	ĜB	�3B	�{B	� B	x�B	u�B	[#B	O�B	;dB	.B	�B	JB	%B�sB�#BɺB�dB�3B�'B�LB�3B�3B�'B�3B�?B�?B�!B�B��B��B��B��B��B�oB�oB��B�B�+B��B�Bw�BiyBn�B�1B��B��B��B�{B�VB�PB�1B�Bv�Bn�Bk�Be`BiyBhsBiyBhsBhsBgmBe`Be`Be`BdZBdZBffBhsBdZBjBhsBffBdZBdZBe`BhsBo�Bv�By�B� B~�B�Bz�B�JB�hB��B��B�{B�uB��B�{B�1B�Bx�Bz�Bo�B[#B\)BN�BG�BE�BC�BK�B@�BF�BC�B?}B:^B5?B5?B7LB8RB;dB<jB8RB49B2-B0!B33B5?B6FB6FB6FB9XB8RB:^B>wBB�BD�BF�BF�BF�BL�BVB[#BbNBcTBe`BiyBn�Bq�Bt�Bv�Bw�By�By�By�B{�B|�B�B�B�B�DB�B�7B��B�JB�VB�\B�PB�PB�VB�JB�VB�JB�JB�hB�\B�PB�PB�VB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B��B�3B�?B�LB�LB�RB�^B�}BÖBǮBɺB��B��B��B�
B�)B�HB�fB�B��B	B	B	B	%B	
=B	JB	VB	VB	hB	uB	uB	�B	�B	"�B	#�B	&�B	/B	0!B	2-B	2-B	5?B	7LB	7LB	9XB	;dB	=qB	?}B	B�B	B�B	C�B	G�B	L�B	O�B	P�B	Q�B	S�B	T�B	T�B	VB	YB	[#B	]/B	_;B	ffB	gmB	hsB	k�B	n�B	p�B	q�B	p�B	q�B	r�B	r�B	r�B	q�B	r�B	s�B	u�B	|�B	~�B	|�B	}�B	� B	~�B	�B	�B	�=B	�DB	�VB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�9B	�?B	�FB	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�jB	�wB	�}B	��B	��B	B	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�NB	�NB	�ZB	�`B	�`B	�`B	�ZB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B
B
DB
�B
"�B
&�B
.B
33B
9XB
?}B
G�B
N�B
R�B
XB
\)B
aHB
gmB
jB
l�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�^B�XB�LB�RB�XB�dB�jB�wB�wB�wB�}B��BÖBŢBƨBǮBǮBǮBȴBɺBɺB��B��B�B��B  BB+B{B<jBM�B[#BdZBffBn�Bu�Bs�Bw�B�B�+B}�B�B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B�XB��B�^B�RB�B��B��B��B�hB�7B�B�bB�PB� Bo�BD�BcTBu�Bw�BT�BDB��B��BB1B��B�B�B�;B��BB�mBB\B{B�B�B�BhB\B��B��B�B�TB�#B�#B�B�B�
B��B��BÖB�'B��B�bB�By�Bs�Bp�BdZBT�BN�BJ�B%�B
��B
��B
�FB
�B
��B
��B
�\B
�\B
�hB
l�B
jB
�B	��B	�dB	��B	�B	�B	|�B	]/B	ZB	C�B	2-B	�B	oB	PB�B�ZB��B��B�RB�FB�jB�FB�9B�'B�3B�LB�^B�9B�!B�!B��B��B��B��B��B�{B��B�+B�=B��B�%Bx�BiyBn�B�=B��B��B��B��B�hB�hB�JB�1B{�Bp�Bn�BjBk�BjBl�BjBjBl�BhsBjBiyBgmBgmBjBl�BiyBn�Bm�BiyBhsBhsBk�Bk�Bp�Bx�B|�B�B�B�By�B�PB�oB��B��B��B��B��B��B�DB�1B{�B� Bv�B_;B^5BO�BH�BG�BF�BM�BC�BI�BF�BB�B<jB7LB6FB8RB8RB<jB>wB:^B6FB49B5?B7LB6FB7LB8RB6FB9XB;dB=qB@�BD�BE�BG�BG�BH�BO�BXB]/BcTBdZBe`BjBo�Br�Bu�Bw�Bx�Bz�Bz�By�B~�B� B�B�%B�+B�PB�B�PB��B�PB�\B�bB�PB�VB�\B�VB�hB�VB�PB��B�oB�VB�\B�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�3B�FB�RB�RB�XB�dB��BĜBǮBɺB��B��B��B�
B�/B�HB�`B�B��B	B	B	B	+B	DB	PB	\B	\B	oB	{B	{B	�B	 �B	"�B	$�B	'�B	0!B	1'B	33B	33B	6FB	8RB	8RB	:^B	<jB	>wB	@�B	C�B	C�B	C�B	H�B	M�B	O�B	P�B	R�B	T�B	VB	VB	W
B	ZB	^5B	^5B	_;B	gmB	hsB	iyB	k�B	n�B	p�B	r�B	q�B	r�B	s�B	s�B	s�B	r�B	s�B	s�B	t�B	|�B	� B	}�B	~�B	�B	~�B	�B	�B	�DB	�DB	�VB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�9B	�FB	�FB	�RB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	�}B	��B	��B	��B	B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�TB	�TB	�`B	�fB	�fB	�fB	�`B	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B
B
JB
�B
"�B
'�B
.B
33B
9XB
?}B
G�B
N�B
R�B
YB
\)B
bNB
gmB
jB
l�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452162012011014521620120110145216  AO  ARGQ                                                                        20111130144204  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144204  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145216  IP                  G�O�G�O�G�O�                