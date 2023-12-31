CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:51Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               *A   AO  20111130143923  20190522121828  1728_5048_042                   2C  D   APEX                            2142                            040306                          846 @ԓ/�1   @ԓ18�@6%�Q��cA��l�D1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D�fD  D� DfD� D  D� D  D� D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dyl�D�fD�9�D�I�D��3D��fD�6fD�s3D���D���D��D�� D�� D��3D��D�Y�D�y�D���D�,�D�Y�D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B~  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C��C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#��C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� CkffCm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cγ3C�� C�� C�� C�� C�� C�� C�� Cֳ3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D�fD` D� D` D� D` D� D` D� D	ffD	� D
` D
� D` D� D` D� DffD� D` D�fD` D� D` D� D` D� D` D�fDffD�fD` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&ٚD'` D'� D(` D(�fD)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DWY�DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� DvY�DyL�D��fD�)�D�9�D��3D��fD�&fD�c3D���D���D��D�p Dǰ D��3D��D�I�D�i�D���D��D�I�D�S3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A�hsA���A�+AύPA�+A��mA�=qA��mA��/A˃A�l�A�&�A���A�=qA��A��TA��
A��-A�ĜA���A�dZA���A���A�S�A�?}A��A�  A��A�O�A��A�1A�A���A�ƨA�dZA��A��-A���A��;A��wA�&�A�ffA�Q�A�A��+A�5?A��HA�I�A��
A�=qA�/A��!A���A�bNA�K�A�JA���A���A���A�Q�A��7A��FA��A��`A�&�A�ffA���A�`BA�$�A��A�%A���A�bNA�x�A��+A��`A��;A��A�(�A��#A�t�A���A���A��A�r�A�ȴA�  A���A��yA�Q�A�VA�$�A���A�1'A���A���A�dZA��uA~��Az9XAsdZAqƨAo�Ak��Ag��AeG�Ad�uAc�Ab�Aa33A_�A\�A[K�AX�RAT�jARjAP~�AM�^AK��AI�AH1AFr�ACp�AA�A@�A>�yA>A�A<bNA:�A9�A6�A5\)A3��A2��A1A/S�A.�A,��A+�7A*ZA(�jA'l�A&ZA%��A%hsA$�jA#�hA#"�A!�TA ZA��A1A�A��A�A��A�+AQ�A?}A�+AbA�wA��A+A~�A��A"�A��A�A�uAn�AVA�AXA^5A��A��AJA"�A�A��A�9A�mA"�A{AA
n�A	�mA�A5?A�FAZA��A;dA~�A��A�7A ȴ@��@�@�I�@���@�@�+@��@�9X@�M�@��/@�;d@�p�@蛦@�M�@噚@�V@�u@��;@��H@�7@߶F@��@ۅ@�V@��H@�v�@�^5@�-@թ�@��@�(�@�@Л�@Ϯ@�"�@θR@�^5@�?}@˅@�ff@�`B@Ȭ@�j@�(�@�ƨ@�@��T@�Ĝ@�A�@�(�@��;@�5?@��`@��@�v�@���@��u@��w@�v�@��@���@�hs@���@���@�dZ@���@�V@���@���@���@�K�@�J@�&�@�  @�l�@�@�V@���@��@���@�A�@��@��@��@�{@�/@�9X@�ƨ@�+@���@�$�@��h@�`B@��/@�Ĝ@���@�1@��@��P@�l�@�;d@�
=@���@��@��y@��H@��@���@���@���@�v�@�E�@�$�@�@���@�p�@���@�Z@�1'@�bN@��D@���@�I�@�  @��P@�@��R@��\@�@��^@��@��D@�Q�@��@���@�\)@�C�@�33@��@�@��y@�ȴ@�v�@�$�@�@���@���@�7L@��u@�Z@�9X@�b@��m@��F@���@���@��P@�|�@�C�@���@�v�@�5?@�{@�J@�J@�J@�J@�J@�{@�{@�{@��@�@�?}@���@���@�j@�9X@��@�ƨ@���@�ƨ@��F@���@�|�@���@��\@�v�@�5?@���@���@�J@�{@�{@��-@�G�@��@�b@��;@��w@���@�S�@�"�@��@�ȴ@��!@���@��\@��+@�n�@�M�@�$�@���@���@�/@���@��`@���@���@���@���@��`@��`@��/@�Ĝ@�I�@��@�;d@�@��R@�^5@�{@���@�p�@�X@�7L@���@���@�9X@�b@���@��m@���@��w@���@�\)@�o@�@��@��R@���@�~�@�5?@�-@��@�-@�@�x�@��D@�j@�Z@��@���@���@��F@���@���@��P@�33@�@��y@���@��+@�E�@���@���@��@��`@|�@qG�@g�P@_��@[o@T9X@Kƨ@C�m@=@8  @1��@,�j@'�@#�F@��@��@�-@bN@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A�hsA���A�+AύPA�+A��mA�=qA��mA��/A˃A�l�A�&�A���A�=qA��A��TA��
A��-A�ĜA���A�dZA���A���A�S�A�?}A��A�  A��A�O�A��A�1A�A���A�ƨA�dZA��A��-A���A��;A��wA�&�A�ffA�Q�A�A��+A�5?A��HA�I�A��
A�=qA�/A��!A���A�bNA�K�A�JA���A���A���A�Q�A��7A��FA��A��`A�&�A�ffA���A�`BA�$�A��A�%A���A�bNA�x�A��+A��`A��;A��A�(�A��#A�t�A���A���A��A�r�A�ȴA�  A���A��yA�Q�A�VA�$�A���A�1'A���A���A�dZA��uA~��Az9XAsdZAqƨAo�Ak��Ag��AeG�Ad�uAc�Ab�Aa33A_�A\�A[K�AX�RAT�jARjAP~�AM�^AK��AI�AH1AFr�ACp�AA�A@�A>�yA>A�A<bNA:�A9�A6�A5\)A3��A2��A1A/S�A.�A,��A+�7A*ZA(�jA'l�A&ZA%��A%hsA$�jA#�hA#"�A!�TA ZA��A1A�A��A�A��A�+AQ�A?}A�+AbA�wA��A+A~�A��A"�A��A�A�uAn�AVA�AXA^5A��A��AJA"�A�A��A�9A�mA"�A{AA
n�A	�mA�A5?A�FAZA��A;dA~�A��A�7A ȴ@��@�@�I�@���@�@�+@��@�9X@�M�@��/@�;d@�p�@蛦@�M�@噚@�V@�u@��;@��H@�7@߶F@��@ۅ@�V@��H@�v�@�^5@�-@թ�@��@�(�@�@Л�@Ϯ@�"�@θR@�^5@�?}@˅@�ff@�`B@Ȭ@�j@�(�@�ƨ@�@��T@�Ĝ@�A�@�(�@��;@�5?@��`@��@�v�@���@��u@��w@�v�@��@���@�hs@���@���@�dZ@���@�V@���@���@���@�K�@�J@�&�@�  @�l�@�@�V@���@��@���@�A�@��@��@��@�{@�/@�9X@�ƨ@�+@���@�$�@��h@�`B@��/@�Ĝ@���@�1@��@��P@�l�@�;d@�
=@���@��@��y@��H@��@���@���@���@�v�@�E�@�$�@�@���@�p�@���@�Z@�1'@�bN@��D@���@�I�@�  @��P@�@��R@��\@�@��^@��@��D@�Q�@��@���@�\)@�C�@�33@��@�@��y@�ȴ@�v�@�$�@�@���@���@�7L@��u@�Z@�9X@�b@��m@��F@���@���@��P@�|�@�C�@���@�v�@�5?@�{@�J@�J@�J@�J@�J@�{@�{@�{@��@�@�?}@���@���@�j@�9X@��@�ƨ@���@�ƨ@��F@���@�|�@���@��\@�v�@�5?@���@���@�J@�{@�{@��-@�G�@��@�b@��;@��w@���@�S�@�"�@��@�ȴ@��!@���@��\@��+@�n�@�M�@�$�@���@���@�/@���@��`@���@���@���@���@��`@��`@��/@�Ĝ@�I�@��@�;d@�@��R@�^5@�{@���@�p�@�X@�7L@���@���@�9X@�b@���@��m@���@��w@���@�\)@�o@�@��@��R@���@�~�@�5?@�-@��@�-@�@�x�@��D@�j@�Z@��@���@���@��F@���@���@��P@�33@�@��y@���@��+@�E�@���@���@��@��`@|�@qG�@g�P@_��@[o@T9X@Kƨ@C�m@=@8  @1��@,�j@'�@#�F@��@��@�-@bN@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB&�B&�B%�B%�B&�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B&�B'�B/B9XB>wBC�BI�BN�BW
B_;Bo�Bx�B}�B|�B�PB��B�B��B��B��B�-B�^B�^B�jB�jB�XB�RB�XBĜBÖBŢB�wB�9B��B�'B�wBƨB��B��B��B�B�B��B��B��BBƨBƨBŢB�}B�LB�?B�B�B��B�B�3B�wB�XB�B��B}�Br�Bn�BhsBXBN�BM�B2-B�B�B{BDB��B�/B��B��B�LB��B�7Bw�Bo�Bl�B^5BH�B;dB33B2-B�B+B
�B
��B
�B
��B
�PB
�B
v�B
k�B
O�B
1'B
{B	�NB	�B	��B	�uB	z�B	R�B	B�B	@�B	5?B	'�B	�B	JB	  B��B�B��BÖB�FB�-B�3B��B��B�{B�bB�%B�=B�PB�hB�B�7Bx�Bq�Bl�Bl�Bk�Bl�Bk�Bk�BhsBffBiyBn�BjBhsBjBk�Bm�Bo�BjBm�Bt�Bn�Br�Bo�Bo�Bo�Bo�Bo�Bo�Bl�BjBk�Bk�BiyBk�BiyBgmBgmBgmBgmBffBffBffBjBhsBdZBbNBjBdZBaHB_;B\)B[#BXBVB\)BVBT�BS�BS�BQ�BP�BN�BK�BK�BG�BE�BD�BB�BA�B?}BA�B;dBF�B@�B6FB8RB7LB49B5?B=qB8RB:^B=qB>wB?}B@�BC�BB�B?}BM�B>wBB�B?}B>wB>wB?}B?}B@�BC�BE�BH�BI�BI�BH�BG�BJ�BI�BJ�BJ�BK�BL�BL�BL�BN�BN�BL�BL�BK�BK�BK�BI�BG�BG�BI�BI�BK�BN�BQ�BQ�BR�BQ�BR�BR�BS�BT�BXBXBVBYB]/B`BBdZBgmBiyBm�Bq�Bu�Bw�B}�B�B�1B�DB�hB��B��B��B��B��B�!B�3B�9B�LB�XB�wBĜBɺB��B��B��B��B��B��B��B��B�B�B�#B�;B�B�B�B�B��B��B	B	B	B	B	B		7B	DB	VB	PB	\B	bB	bB	�B	�B	 �B	%�B	&�B	)�B	,B	.B	/B	0!B	1'B	2-B	49B	5?B	8RB	=qB	A�B	D�B	D�B	I�B	O�B	Q�B	R�B	VB	YB	[#B	]/B	^5B	`BB	aHB	dZB	jB	o�B	r�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	w�B	y�B	|�B	�B	�B	�B	�B	�+B	�1B	�7B	�7B	�7B	�DB	�JB	�VB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�-B	�3B	�3B	�3B	�9B	�LB	�XB	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�wB	�}B	B	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�BB	�BB	�HB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�yB	��B
	7B
{B
�B
#�B
+B
33B
:^B
A�B
G�B
N�B
R�B
[#B
^5B
dZB
iyB
o�B
u�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B&�B&�B%�B%�B&�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B'�B)�B1'B<jBA�BE�BJ�BQ�BXBcTBt�B|�B�B�B�oB��B�'B�!B��B��B�FB�qB�jB�wB�wB�^B�XB�^BƨBȴBǮB��B�LB��B�9B��BǮB��B��B�B�/B�/B�
B��B��BÖBȴB��BȴBB�dB�dB�!B�'B��B�B�?B��B�qB�9B��B�Bw�Bs�Bm�B[#BQ�BP�B7LB�B�B�BbBB�HB��BĜB�jB�B�PBy�Bq�Bo�Be`BK�B?}B7LB7LB�BPB
��B
�B
�-B
��B
�bB
�B
z�B
t�B
[#B
>wB
"�B	��B	�'B	��B	��B	�B	XB	D�B	B�B	8RB	+B	#�B	hB	B��B��B�BȴB�qB�LB�RB��B��B��B��B�7B�VB�\B��B�7B�\B� Bu�Bq�Bo�Bp�Bq�Bo�Bo�Bl�BjBn�Br�Bm�BjBk�Bm�Bp�Bp�Bm�Bq�Bv�Bs�Bu�Bp�Bp�Bp�Bp�Bp�Bs�Bn�Bl�Bl�Bl�Bk�Bm�Bk�BjBhsBhsBhsBgmBgmBhsBm�Bl�BgmBffBm�BgmBcTBcTB`BB^5B[#BZB`BBXBW
BXBW
BS�BVBP�BM�BN�BL�BJ�BG�BF�BD�BC�BF�BB�BK�BB�B7LB:^B9XB6FB7LB>wB;dB;dB>wB?}B@�BB�BE�BE�BC�BP�BB�BE�B@�B?}B?}B@�B@�BB�BG�BH�BJ�BJ�BJ�BI�BI�BM�BK�BL�BL�BL�BM�BM�BN�BP�BP�BM�BM�BK�BO�BN�BK�BJ�BJ�BJ�BK�BN�BN�BR�BR�BT�BS�BT�BS�BVBT�BZBZBVB\)B_;BcTBffBhsBk�Bn�Bq�Bu�Bw�B~�B�B�7B�PB�uB��B��B��B��B��B�'B�9B�?B�RB�^B�}BŢB��B��B��B��B��B��B��B��B��B�
B�B�#B�BB�B�B�B�B��B	  B	B	B	B	B	B		7B	JB	\B	VB	bB	hB	hB	�B	�B	!�B	&�B	'�B	+B	-B	.B	/B	0!B	1'B	2-B	5?B	6FB	9XB	>wB	B�B	E�B	E�B	J�B	P�B	R�B	S�B	W
B	ZB	[#B	]/B	^5B	`BB	bNB	e`B	k�B	p�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	u�B	x�B	z�B	}�B	�B	�B	�B	�%B	�1B	�1B	�7B	�7B	�7B	�JB	�PB	�\B	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�3B	�3B	�9B	�9B	�9B	�?B	�LB	�XB	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�wB	��B	ÖB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�
B	�B	�
B	�
B	�B	�
B	�
B	�B	�
B	�
B	�B	�#B	�#B	�#B	�#B	�/B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�mB	�yB	��B
	7B
{B
�B
$�B
+B
33B
:^B
A�B
G�B
N�B
R�B
\)B
_;B
e`B
iyB
p�B
u�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<e`B<���<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452072012011014520720120110145207  AO  ARGQ                                                                        20111130143923  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143923  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145207  IP                  G�O�G�O�G�O�                