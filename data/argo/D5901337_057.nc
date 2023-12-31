CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:39Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               9A   AO  20111205113317  20190522121836  1901_5055_057                   2C  D   APEX                            2140                            040306                          846 @Է�i��1   @ԷI��@-�-�cp��E�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dv��Dy�3D�fD�  D�i�D�� D�� D�L�D�` D�ɚD�� D�#3D�Y�DǶfD���D��D�l�D� D��fD�  D�C3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@���@�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BNffBV  B]��Bf  Bn  Bv  B~  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
ٚD` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+�fD,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?ffD?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^�fD_ffD_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du�fDv` Dv��Dys3D��fD� D�Y�D�� D�� D�<�D�P D���D�� D�3D�I�DǦfD���D��D�\�D� D��fD� D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�r�A�t�A�v�A�x�A�x�A�x�A�v�A�v�A�t�A�z�A�|�A�~�AŁAŃAŁAŃAŃAŅAŅAŉ7AŇ+Aŉ7AŇ+Aŉ7Aŉ7AōPAœuAœuAœuAŕ�Aŕ�Aŗ�Ař�Ař�Aś�Aś�Aş�Aţ�A�z�A�E�A�%A�XA��A�
=A��A�A��DA�C�A�jA�l�A��#A�O�A�XA��HA�bA�bNA��7A���A�$�A�C�A�|�A�(�A�Q�A�A�VA�;dA�9XA�5?A�C�A�&�A���A��
A���A��#A���A�n�A���A��PA�=qA��
A�K�A��7A��TA�p�A�bNA�M�A}��Ay��Av�+AsoAi��Ad�`Ab �A]�A[?}AX��AT�`AQ�#AO�
AN  AL��AJr�AF�AD�AA
=A=`BA:A�A8Q�A6bA4�!A3�A2�/A1oA0��A0��A0M�A/��A/oA/VA/�#A0�/A1oA/O�A.A�A.1A-��A-C�A+C�A*�\A*=qA*I�A*=qA)�7A(�A(VA'|�A&�A&M�A#��A"��A"�A!�A!��A!+A�A�A   AK�A`BA E�A r�A�TA?}A�A~�AZAJA��A�wAt�AC�A�A�yAA�AA�-A�AO�A�A��A{A�A��A��A�A��Az�AA��A`BA�HA��AQ�A&�AffA(�A1A�Al�AjAXAffAZAJA�A�`Ar�A �A��AG�A/A�A��A��A�A �A�A|�AXAS�A+A
�HA
Q�A	��A	`BA��Av�A��AK�A�!A1'A��A7LAJA�hAO�A�A��A=qA��A �A ��A z�A jA I�A  �@��F@�^5@��@���@��m@�"�@��R@�-@��T@�&�@�j@�b@�C�@�^5@�hs@�X@���@�S�@�R@�ff@��@�7@��@���@��;@@��@�@�z�@�\)@���@�E�@���@陚@�?}@���@�+@��#@��@� �@�l�@◍@��#@���@��@ߝ�@�"�@ޏ\@��T@�/@�Q�@ۮ@ڗ�@ٺ^@�7L@���@�z�@� �@�+@�v�@���@�G�@�Z@�;d@�ȴ@�n�@��@���@ёh@��@��
@υ@�V@�G�@̴9@�bN@�  @˝�@�@ʏ\@�5?@��@�X@��`@ȋD@�ƨ@�+@���@�~�@��T@ř�@�`B@�%@��/@�I�@��;@�l�@���@�=q@��@�&�@���@�(�@�ƨ@�|�@�
=@�v�@���@���@�Z@�t�@��@�V@��h@���@���@�b@��@��@�@��H@��\@��-@��7@�p�@���@��j@�r�@�I�@��
@�t�@�
=@���@��!@�~�@�-@�@�@�?}@���@�Q�@�  @�l�@�"�@��\@�{@��T@��7@�x�@�`B@�?}@�V@��`@��@�Z@��;@��w@���@�t�@�C�@��H@�=q@��T@��h@��@�V@��`@��/@���@���@�Q�@��@���@�C�@��@��@��+@��7@�Ĝ@��u@�Z@�b@�l�@�o@���@�ff@�J@�@�`B@�?}@�%@���@��@�1'@�  @��w@�t�@�\)@�"�@��@��+@�^5@�$�@��T@���@�?}@��/@�Ĝ@��D@�  @��
@���@��@�V@���@��^@��h@�V@��u@�(�@��m@��@�dZ@��@���@���@��+@�=q@��^@�`B@��9@�bN@�A�@� �@�ƨ@���@��P@�S�@��@�$�@��h@�?}@�%@��`@���@��9@��u@�r�@�Q�@��w@��@�K�@�o@��@���@�v�@��@��D@�Q�@w�@m?}@c��@X��@Q��@J-@D�D@=p�@6E�@0A�@)��@"�@��@%@(�@Q�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�n�A�r�A�t�A�v�A�x�A�x�A�x�A�v�A�v�A�t�A�z�A�|�A�~�AŁAŃAŁAŃAŃAŅAŅAŉ7AŇ+Aŉ7AŇ+Aŉ7Aŉ7AōPAœuAœuAœuAŕ�Aŕ�Aŗ�Ař�Ař�Aś�Aś�Aş�Aţ�A�z�A�E�A�%A�XA��A�
=A��A�A��DA�C�A�jA�l�A��#A�O�A�XA��HA�bA�bNA��7A���A�$�A�C�A�|�A�(�A�Q�A�A�VA�;dA�9XA�5?A�C�A�&�A���A��
A���A��#A���A�n�A���A��PA�=qA��
A�K�A��7A��TA�p�A�bNA�M�A}��Ay��Av�+AsoAi��Ad�`Ab �A]�A[?}AX��AT�`AQ�#AO�
AN  AL��AJr�AF�AD�AA
=A=`BA:A�A8Q�A6bA4�!A3�A2�/A1oA0��A0��A0M�A/��A/oA/VA/�#A0�/A1oA/O�A.A�A.1A-��A-C�A+C�A*�\A*=qA*I�A*=qA)�7A(�A(VA'|�A&�A&M�A#��A"��A"�A!�A!��A!+A�A�A   AK�A`BA E�A r�A�TA?}A�A~�AZAJA��A�wAt�AC�A�A�yAA�AA�-A�AO�A�A��A{A�A��A��A�A��Az�AA��A`BA�HA��AQ�A&�AffA(�A1A�Al�AjAXAffAZAJA�A�`Ar�A �A��AG�A/A�A��A��A�A �A�A|�AXAS�A+A
�HA
Q�A	��A	`BA��Av�A��AK�A�!A1'A��A7LAJA�hAO�A�A��A=qA��A �A ��A z�A jA I�A  �@��F@�^5@��@���@��m@�"�@��R@�-@��T@�&�@�j@�b@�C�@�^5@�hs@�X@���@�S�@�R@�ff@��@�7@��@���@��;@@��@�@�z�@�\)@���@�E�@���@陚@�?}@���@�+@��#@��@� �@�l�@◍@��#@���@��@ߝ�@�"�@ޏ\@��T@�/@�Q�@ۮ@ڗ�@ٺ^@�7L@���@�z�@� �@�+@�v�@���@�G�@�Z@�;d@�ȴ@�n�@��@���@ёh@��@��
@υ@�V@�G�@̴9@�bN@�  @˝�@�@ʏ\@�5?@��@�X@��`@ȋD@�ƨ@�+@���@�~�@��T@ř�@�`B@�%@��/@�I�@��;@�l�@���@�=q@��@�&�@���@�(�@�ƨ@�|�@�
=@�v�@���@���@�Z@�t�@��@�V@��h@���@���@�b@��@��@�@��H@��\@��-@��7@�p�@���@��j@�r�@�I�@��
@�t�@�
=@���@��!@�~�@�-@�@�@�?}@���@�Q�@�  @�l�@�"�@��\@�{@��T@��7@�x�@�`B@�?}@�V@��`@��@�Z@��;@��w@���@�t�@�C�@��H@�=q@��T@��h@��@�V@��`@��/@���@���@�Q�@��@���@�C�@��@��@��+@��7@�Ĝ@��u@�Z@�b@�l�@�o@���@�ff@�J@�@�`B@�?}@�%@���@��@�1'@�  @��w@�t�@�\)@�"�@��@��+@�^5@�$�@��T@���@�?}@��/@�Ĝ@��D@�  @��
@���@��@�V@���@��^@��h@�V@��u@�(�@��m@��@�dZ@��@���@���@��+@�=q@��^@�`B@��9@�bN@�A�@� �@�ƨ@���@��P@�S�@��@�$�@��h@�?}@�%@��`@���@��9@��u@�r�@�Q�@��w@��@�K�@�o@��@���@�v�@��@��D@�Q�@w�@m?}@c��@X��@Q��@J-@D�D@=p�@6E�@0A�@)��@"�@��@%@(�@Q�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�yB�yB�yB�yB�yB�yB�yB�yB�yB�B�B�yB�yB�yB�yB�B�B�B�yB�B�yB�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B	B	��B
��B��B��B�!B��B��B��B�?B�qBĜB�LB��B�hB�\B�LB��B��BɺBÖB�}B�LB�B��B��B�hB�VB�7Bw�BffBF�B1'B+B�BB
��B
��B
��B
�ZB
�qB
�PB
w�B
l�B
^5B
J�B
�B	��B	�TB	��B	�3B	��B	�B	M�B	9XB	(�B	JB	B��B�B�TB�)B��B��BƨB�qB�!B�B��B��B��B��B��B�{B�{B��B��B��B��B��B�3BB�B	�B	7LB	G�B	S�B	_;B	x�B	�B	�uB	��B	��B	��B	�!B	�-B	�qB	ȴB	��B	��B	ɺB	ƨB	��B	�B	�B	�#B	�BB	�mB	�B
  B	��B
%B
�B
�B
#�B
(�B
,B
/B
0!B
2-B
49B
49B
7LB
8RB
:^B
;dB
@�B
A�B
C�B
D�B
E�B
G�B
H�B
I�B
K�B
K�B
K�B
J�B
J�B
J�B
I�B
G�B
E�B
C�B
B�B
A�B
<jB
9XB
8RB
8RB
8RB
7LB
1'B
-B
)�B
(�B
&�B
$�B
%�B
&�B
(�B
&�B
)�B
+B
+B
,B
-B
,B
+B
-B
-B
,B
/B
1'B
/B
-B
)�B
&�B
%�B
&�B
#�B
 �B
�B
�B
�B
�B
�B
�B
�B
uB
oB
\B
VB
PB
PB
PB
PB
PB
JB

=B
	7B
	7B
	7B
1B
+B
+B
+B
%B
%B
%B
%B
B
%B
+B
+B
%B
B
%B
B
%B
%B
%B
%B
B
B
B
B
  B	��B
  B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B
DB

=B

=B

=B
DB
DB
DB
DB

=B
DB
JB
DB
DB
JB
JB
JB
JB
JB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
#�B
+B
0!B
5?B
<jB
D�B
H�B
N�B
R�B
XB
\)B
aHB
dZB
iyB
n�B
q�B
v�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�yB�yB�yB�yB�yB�yB�yB�yB�yB�B�B�yB�yB�yB�yB�B�B�B�yB�B�yB�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B	B	��B
��B��B�?B�RBƨB��B��B�RBÖB��B�XB��B��B�hB�FB��B��B��BƨBĜB��B�9B�B�B�uB�\B�VB|�Bp�BQ�B33B1'B'�B
=B  B
��B
��B
��B
��B
�uB
z�B
p�B
aHB
VB
-B
B	�B	�/B	�dB	��B	��B	[#B	?}B	1'B	bB	
=B	B��B�sB�HB�B��B��BĜB�XB�FB�B��B��B��B��B��B��B��B��B��B��B��B�3B��B�B	�B	;dB	J�B	T�B	_;B	z�B	�7B	��B	��B	��B	��B	�-B	�9B	�}B	��B	��B	��B	��B	��B	��B	�
B	�B	�/B	�ZB	�yB	�B
B	��B
B
�B
!�B
%�B
+B
-B
0!B
1'B
33B
49B
5?B
8RB
9XB
;dB
=qB
A�B
B�B
D�B
E�B
F�B
H�B
J�B
L�B
L�B
K�B
K�B
K�B
K�B
L�B
K�B
H�B
G�B
D�B
C�B
E�B
?}B
:^B
9XB
9XB
:^B
:^B
49B
0!B
)�B
)�B
(�B
&�B
'�B
'�B
)�B
(�B
+B
+B
,B
-B
.B
-B
,B
/B
.B
,B
0!B
2-B
1'B
/B
,B
(�B
'�B
)�B
%�B
"�B
 �B
�B
�B
�B
�B
�B
�B
{B
{B
oB
hB
VB
VB
VB
VB
VB
PB
PB
JB

=B
DB

=B
1B
1B
1B
+B
+B
+B
+B
%B
1B
+B
1B
1B
%B
+B
%B
+B
+B
+B
1B
+B
%B
B
B
B
  B
B
  B
  B
  B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
  B
B
B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
+B
%B
+B
%B
+B
+B
+B
1B

=B
JB
DB
DB
DB
JB
JB
JB
PB
JB
JB
PB
JB
JB
PB
PB
PB
PB
PB
\B
bB
bB
bB
bB
bB
bB
bB
bB
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
uB
uB
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
#�B
+B
0!B
5?B
<jB
D�B
H�B
N�B
S�B
XB
\)B
aHB
dZB
iyB
n�B
r�B
v�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<D��<#�
<#�
<#�
<#�
<�o<u<#�
<#�
<#�
<#�
<49X<u<#�
<#�
<49X<#�
<#�
<ě�<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250272012011312502720120113125027  AO  ARGQ                                                                        20111205113317  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113317  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125027  IP                  G�O�G�O�G�O�                