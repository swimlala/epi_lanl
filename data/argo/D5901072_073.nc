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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               IA   AO  20111130144243  20190522121829  1728_5048_073                   2C  D   APEX                            2142                            040306                          846 @��'� 1   @��(DD@@6�$�/�b�I�^1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dvs3Dy` D�3D�33D�s3D��3D�fD�@ D�\�D���D�� D��D��3DǼ�D��fD�  D�VfD��3D�� D�fD�L�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @ff@`  @�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B~  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	ffC� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co��Cq��Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C��3C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cֳ3C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DFٚDG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` DsٚDt` Dt� Du` Du� DvS3Dy@ D��3D�#3D�c3D��3D��fD�0 D�L�D���D�� D�	�D�s3DǬ�D��fD� D�FfD�3D�� D�fD�<�D�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�VA�\)A�S�A�S�A�E�A��A�A�5?A�/A�A�A��/A��A��;A�VA��A²-A�ZA�1'A� �A� �A�-A��A��^A��A��;A�C�A��`A��RA�=qA���A�ȴA���A��-A�=qA��RA�bNA�&�A�l�A��A�1'A��!A��7A�bA�;dA��A��A�9XA�p�A��hA���A��A�l�A�&�A���A�(�A��jA�bNA��A�z�A��wA��uA�|�A�/A��A�dZA���A�/A�5?A���A�l�A��jA�=qA�l�A��;A���A�jA��RA��hA�K�A���A��hA�ȴA� �A��\A��jA�z�A���A��DA�ȴA��9A��A�1'A��mA���A��\A�ZA�v�A�n�A�1A��A��#A���A�r�A�ZA�&�A�z�A�oA�ȴA�JA���A�r�A���Ap�A|�Ax�Au��Ar��AqoAk�Ag�^AgdZAg
=Af�!AfJAe33AdE�Ab~�Aa��A_VA[��AY��AW\)AUt�AS�ARQ�AP�uAN��AM�PAL�AK�^AJM�AI��AH�AF��AE�-AD�HAC��AA��A?�A>�A<��A<jA<�A:�`A9t�A8M�A6�/A5p�A4��A4��A3�;A2ffA1"�A0JA.�A-�7A,��A+�A*�A*5?A)��A(�9A'dZA&ĜA%�A%�A$��A#�mA#O�A!�A ��A ZA|�AI�A?}AJAt�A^5A�A�9A{A/AhsA�yAn�A|�A��AZA�
Av�A33AƨAĜAA
�/A
ZA	��A	33A�+A�-A�DAG�An�A/AQ�A+A Q�@�ȴ@��u@�S�@���@���@�ff@��@�`B@��u@�K�@�{@�O�@�Q�@��@���@��@@�^5@�@�F@�5?@�z�@��#@�1@�J@�&�@���@��D@�r�@�l�@��#@�z�@ە�@�5?@�x�@�&�@ؼj@�t�@���@Ԭ@��@��`@ϕ�@�ff@�O�@�b@���@��#@�V@�  @�\)@��@���@�~�@�@ź^@ŉ7@�7L@�V@�/@�hs@�7L@��/@ēu@��m@��@�ff@���@��7@���@���@���@�/@��u@��;@�+@��!@�v�@�M�@�x�@���@��F@��!@�n�@�ȴ@�@��@��R@��@���@��@�p�@��7@�&�@���@���@�A�@��m@��w@�33@���@���@�E�@�hs@���@��u@���@��-@���@�l�@�J@�G�@�9X@�t�@��P@��@���@�O�@���@� �@��@�t�@�ȴ@��@�Q�@�Ĝ@��;@�
=@��@��@�l�@���@��@��-@�X@���@�j@�  @��@�;d@��R@�$�@��T@�@��@�@��h@���@���@��@�G�@�7L@���@�Ĝ@�Ĝ@��j@�I�@��
@���@�t�@�C�@��@���@�{@���@���@��7@�`B@�?}@�%@��@�Q�@�1'@�b@��w@�|�@��P@���@�+@��y@���@��H@��H@��+@��@���@��h@���@��h@��7@�p�@�O�@�&�@���@�Ĝ@��D@�Q�@�  @��;@��@�l�@��@���@���@�^5@�$�@���@��T@��^@�p�@�&�@��`@�z�@�Q�@�1'@� �@���@���@�|�@�dZ@�S�@�;d@��@��y@��\@�5?@�J@��@��T@��#@�@���@��7@�X@��@���@��/@�Ĝ@���@��@�I�@�(�@�b@�ƨ@��@�S�@�o@��y@���@���@��\@�ff@�5?@�{@���@��#@���@�V@��j@��@���@�z�@�bN@�A�@� �@�1@��@��;@�ƨ@��@�t�@�K�@�"�@�@�$�@z=q@q��@k�@eV@[�
@Up�@N5?@HQ�@@��@:�@6ȴ@1G�@+�m@%@ A�@�@��@C�@�@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�XA�VA�\)A�S�A�S�A�E�A��A�A�5?A�/A�A�A��/A��A��;A�VA��A²-A�ZA�1'A� �A� �A�-A��A��^A��A��;A�C�A��`A��RA�=qA���A�ȴA���A��-A�=qA��RA�bNA�&�A�l�A��A�1'A��!A��7A�bA�;dA��A��A�9XA�p�A��hA���A��A�l�A�&�A���A�(�A��jA�bNA��A�z�A��wA��uA�|�A�/A��A�dZA���A�/A�5?A���A�l�A��jA�=qA�l�A��;A���A�jA��RA��hA�K�A���A��hA�ȴA� �A��\A��jA�z�A���A��DA�ȴA��9A��A�1'A��mA���A��\A�ZA�v�A�n�A�1A��A��#A���A�r�A�ZA�&�A�z�A�oA�ȴA�JA���A�r�A���Ap�A|�Ax�Au��Ar��AqoAk�Ag�^AgdZAg
=Af�!AfJAe33AdE�Ab~�Aa��A_VA[��AY��AW\)AUt�AS�ARQ�AP�uAN��AM�PAL�AK�^AJM�AI��AH�AF��AE�-AD�HAC��AA��A?�A>�A<��A<jA<�A:�`A9t�A8M�A6�/A5p�A4��A4��A3�;A2ffA1"�A0JA.�A-�7A,��A+�A*�A*5?A)��A(�9A'dZA&ĜA%�A%�A$��A#�mA#O�A!�A ��A ZA|�AI�A?}AJAt�A^5A�A�9A{A/AhsA�yAn�A|�A��AZA�
Av�A33AƨAĜAA
�/A
ZA	��A	33A�+A�-A�DAG�An�A/AQ�A+A Q�@�ȴ@��u@�S�@���@���@�ff@��@�`B@��u@�K�@�{@�O�@�Q�@��@���@��@@�^5@�@�F@�5?@�z�@��#@�1@�J@�&�@���@��D@�r�@�l�@��#@�z�@ە�@�5?@�x�@�&�@ؼj@�t�@���@Ԭ@��@��`@ϕ�@�ff@�O�@�b@���@��#@�V@�  @�\)@��@���@�~�@�@ź^@ŉ7@�7L@�V@�/@�hs@�7L@��/@ēu@��m@��@�ff@���@��7@���@���@���@�/@��u@��;@�+@��!@�v�@�M�@�x�@���@��F@��!@�n�@�ȴ@�@��@��R@��@���@��@�p�@��7@�&�@���@���@�A�@��m@��w@�33@���@���@�E�@�hs@���@��u@���@��-@���@�l�@�J@�G�@�9X@�t�@��P@��@���@�O�@���@� �@��@�t�@�ȴ@��@�Q�@�Ĝ@��;@�
=@��@��@�l�@���@��@��-@�X@���@�j@�  @��@�;d@��R@�$�@��T@�@��@�@��h@���@���@��@�G�@�7L@���@�Ĝ@�Ĝ@��j@�I�@��
@���@�t�@�C�@��@���@�{@���@���@��7@�`B@�?}@�%@��@�Q�@�1'@�b@��w@�|�@��P@���@�+@��y@���@��H@��H@��+@��@���@��h@���@��h@��7@�p�@�O�@�&�@���@�Ĝ@��D@�Q�@�  @��;@��@�l�@��@���@���@�^5@�$�@���@��T@��^@�p�@�&�@��`@�z�@�Q�@�1'@� �@���@���@�|�@�dZ@�S�@�;d@��@��y@��\@�5?@�J@��@��T@��#@�@���@��7@�X@��@���@��/@�Ĝ@���@��@�I�@�(�@�b@�ƨ@��@�S�@�o@��y@���@���@��\@�ff@�5?@�{@���@��#@���@�V@��j@��@���@�z�@�bN@�A�@� �@�1@��@��;@�ƨ@��@�t�@�K�@�"�@�@�$�@z=q@q��@k�@eV@[�
@Up�@N5?@HQ�@@��@:�@6ȴ@1G�@+�m@%@ A�@�@��@C�@�@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�
B�B��B��B��B��B��BƨBŢBǮBB�qB�qB�wBŢB�BB�B�B�B��B��B��B��B��BBB+B1BPB{BoBoBoB	7BuB�B�B�B�B"�B�B�B\BbB�B�B�B�B�B$�B �B-B-B+B2-B2-B0!B/B1'B+B�B��B�TBĜB��B�Bv�Bl�BhsBgmBq�B��B�dB�jB�}B��B�+Bv�BD�B�B��BŢB�B��B�hB�BcTBE�B?}B=qB7LB(�B�BhB+B
��B
�mB
��B
��B
�'B
��B
��B
�7B
z�B
s�B
gmB
\)B
8RB
!�B
VB	��B	�yB	�B	��B	�uB	��B	��B	��B	��B	��B	��B	�hB	�PB	p�B	YB	K�B	5?B	+B	�B	hB		7B	  B��B�B�B�mB�HB�B��B��BɺBŢB�^B�RB�B�B�'B�wBÖB�qB�wB�jB��B��B��B��B��B�B��B�bB��B�VB�JB�7B�JB�PB�JB��B�7B�B�Bw�B{�Bs�Bs�Bp�Bt�Bt�Bq�BffBx�BdZBe`BbNBcTB`BB\)B[#BT�BS�BS�BQ�BQ�BR�BP�BR�BQ�BL�BK�BK�BL�BM�BL�BK�BL�BN�BL�BH�BF�BE�BC�BG�BN�BK�B?}B?}B?}BC�BI�BH�BM�BZBS�BP�BR�BVBQ�BP�BcTBM�BH�BM�BL�BN�BH�BE�BC�BD�BC�BF�BG�BH�BJ�BO�BJ�BM�BN�BN�BN�BM�BN�BN�BM�BL�BN�BN�BO�BP�BR�BXBYB[#B]/BaHBe`BhsBl�Bk�Bl�Bo�Bp�Bw�B� B�B�7B�+B�+B�7B�VB�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�RB�wBȴB��BɺB��B��B��B�B�B�)B�5B�NB�ZB�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B	  B��B	B	B	  B	B	B	PB	�B	�B	 �B	"�B	 �B	�B	�B	�B	�B	 �B	#�B	#�B	%�B	&�B	)�B	-B	.B	49B	8RB	:^B	@�B	C�B	D�B	G�B	H�B	I�B	I�B	M�B	Q�B	S�B	VB	XB	_;B	^5B	`BB	bNB	bNB	cTB	e`B	gmB	iyB	l�B	m�B	p�B	r�B	u�B	w�B	z�B	|�B	~�B	�B	�%B	�1B	�PB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�FB	�LB	�RB	�^B	�qB	��B	�}B	B	B	ĜB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
%B
VB
�B
�B
&�B
-B
33B
:^B
@�B
F�B
J�B
Q�B
W
B
]/B
bNB
gmB
jB
o�B
r�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�#B�#B�
B�B�B�B��BȴBƨBɺBÖB�wB�qB�wBŢB�HB�B�B��B��B��B��B��BBB+BJBDBbB�BuB�B{BbB�B�B�B�B#�B$�B�B�BuB{B�B�B�B�B�B&�B"�B/B1'B/B33B33B2-B1'B5?B0!B$�BB�B��B��B�%B{�Bp�BjBhsBp�B��B�qB�}BĜB��B�=B|�BK�B �B�BɺB�-B�B��B�7Bn�BG�B@�B?}B<jB/B�B{BDB%B
�B
�B
��B
�?B
�B
��B
�VB
}�B
z�B
p�B
dZB
@�B
/B
�B
B	�B	�`B	�9B	�{B	��B	��B	��B	��B	��B	��B	�uB	�{B	x�B	_;B	R�B	;dB	0!B	!�B	�B	VB	B��B��B�B�B�fB�5B�B��B��B��B��B�wB�!B�!B�-BBȴB��BÖB��B�B��B��B��B��B�'B��B�{B��B�bB�\B�DB�PB�bB�bB��B�JB�B�By�B}�Bw�Bv�Br�Bw�Bx�Bt�BjBz�BhsBhsBe`Be`BcTBaHB]/BW
BW
BW
BS�BS�BW
BT�BXBT�BP�BN�BM�BO�BO�BO�BN�BP�BR�BP�BL�BI�BH�BE�BI�BQ�BM�B@�B@�B@�BD�BJ�BI�BO�B\)BVBR�BS�BXBS�BT�Be`BO�BK�BP�BO�BR�BK�BH�BE�BE�BD�BG�BI�BK�BL�BQ�BM�BO�BO�BO�BP�BP�BP�BQ�BP�BN�BP�BP�BQ�BR�BT�BZB[#B\)B^5BbNBffBiyBm�Bl�Bm�Bp�Bp�Bw�B�B�%B�=B�1B�7B�=B�\B�{B�uB��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�'B�XB�}B��B��B��B��B��B��B�
B�#B�/B�;B�TB�`B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B	B��B	B	B	B	B	B	DB	�B	!�B	"�B	$�B	#�B	�B	�B	�B	�B	!�B	$�B	$�B	&�B	'�B	+B	.B	/B	5?B	9XB	:^B	A�B	D�B	D�B	G�B	H�B	J�B	I�B	N�B	Q�B	S�B	VB	YB	`BB	_;B	aHB	cTB	cTB	dZB	ffB	hsB	jB	l�B	n�B	q�B	s�B	v�B	x�B	{�B	}�B	� B	�B	�%B	�1B	�VB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�LB	�RB	�XB	�dB	�wB	��B	��B	ÖB	ÖB	ĜB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�/B	�5B	�BB	�;B	�HB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
%B
VB
�B
�B
&�B
-B
33B
;dB
A�B
F�B
K�B
Q�B
XB
]/B
cTB
gmB
k�B
o�B
r�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452182012011014521820120110145218  AO  ARGQ                                                                        20111130144243  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144243  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145218  IP                  G�O�G�O�G�O�                