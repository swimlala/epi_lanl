CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               -A   AO  20111130143943  20190522121829  1728_5048_045                   2C  D   APEX                            2142                            040306                          846 @Ԛ���@1   @Ԛ�)���@6Z�G�{�c1�S���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy� D�3D�33D�l�D���D���D�0 D�ffD���D�3D�,�D��fDǰ D��fD�9�D�ffD��D��D��D�` D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��@���@�  A  A8  AX  Ax  A�  A�  A�  A�33A�33A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B~  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C��C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co��Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� DjffDj�fDk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� DwY�Dy� D��3D�#3D�\�D���D��D�  D�VfD���D��3D��D�vfDǠ D��fD�)�D�VfD���D�ٚD�	�D�P D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�^5A�^5A�bNA�bNA�bNA�bNA�ffA�hsA�hsA�hsA�hsA�jA�l�A�n�A�n�A�p�A�r�A�p�A�n�A�r�A�n�A�n�A�l�A�l�A�jA�ffA�\)A�M�A�G�A�E�A�C�A�?}A�9XA�bA��A�A��A�A�x�A�r�A��A��A���A���A��
A���A�ȴA�VA��-A�n�A�A��-A���A��jA�{A��/A�z�A��A��A�A���A�M�A�Q�A��A���A�\)A�bA���A��
A�VA���A���A�?}A��A��A��A�K�A�/A�$�A��`A�E�A�{A�E�A���A�K�A��A���A�%A�x�A��!A��A���A�1A��A��PA���A���A�l�A��DA�n�A�z�A���A��9A���A��A��/A��/A�1A��!A��RA�
=A��uA�dZA�M�A�r�A�A~��AyK�AxVAsK�An1'Ah��Ah^5Ae��Aa�A_�AY��AX�/AW7LAT�+AS�-AS%ARI�ANȴAI�TAH�AG��AFbNAF�AEp�ACp�AA��A>bNA<�uA<r�A<�A:�uA8�jA7�hA6�RA5�wA4Q�A3+A1�#A1�A0jA/XA.M�A-\)A,��A+��A+�A)VA'�#A&��A&1A%�
A%\)A$�/A$��A$bNA$(�A#�#A#|�A#�A"�jA"5?A!�-A|�AJA�TA�-A33A�A^5A��A�7A�A=qA��A�jA�
A\)A�A��A1AS�A1'A|�A33A�A��A1AQ�AbA�#AhsA=qA1At�A
�A
(�A�A�
AoA�A^5A�TA\)A��A�^A��A=qA�mA�A �j@�K�@�x�@��9@��m@�;d@�n�@�/@��@�1@�"�@�O�@�@�`B@睲@�x�@�!@�l�@܋D@�K�@��#@�V@���@��@�9X@�=q@��;@��@Ѻ^@�C�@�A�@�z�@�|�@ǍP@��y@�5?@�{@�&�@Õ�@�=q@���@��7@��@�A�@���@�1'@�(�@�(�@��
@���@�33@�ȴ@���@���@��m@��F@�t�@��@�$�@�j@��F@�C�@�33@�l�@��@�I�@��\@�r�@��@�dZ@�+@�ff@��w@��
@�`B@��@��H@�{@�v�@�=q@��@��/@�dZ@�o@��+@��\@���@�@�X@��@���@�9X@�X@�=q@��@�X@�p�@�ȴ@���@���@�K�@�t�@�@���@�E�@�{@��@�$�@�@���@�X@�@�hs@�`B@�X@�X@��`@�K�@�@�r�@�C�@�V@�$�@�@�1@�ȴ@�v�@�=q@��@��9@��D@�  @���@�v�@�-@���@�hs@�V@��`@��j@�r�@�I�@�Q�@���@�
=@���@��+@�E�@��^@�`B@�%@��`@��u@�9X@�1'@�(�@�  @��
@���@�C�@��!@�ȴ@��@�^5@�E�@��@��T@�@���@�p�@�O�@��/@��@��D@��u@���@���@��9@���@���@��/@���@��@��@�Q�@� �@�1@��@���@���@�l�@��y@���@���@���@���@��+@�v�@�^5@��@�j@��@��P@���@���@�K�@��@��y@���@�ff@�M�@�M�@��+@�E�@�/@�?}@�G�@�`B@�x�@�&�@� �@�ƨ@�ƨ@��F@�dZ@�
=@�ȴ@���@���@��H@�33@�S�@�S�@�33@��@��@���@��!@��+@�n�@�V@��@��@��T@�`B@�V@��@���@��9@�Q�@���@�S�@�+@��@��@�o@�
=@���@��+@�=q@���@���@��7@�?}@�%@���@�bN@� �@�|�@|j@s�F@kC�@d��@_K�@Xr�@OK�@I��@D�@<�@6E�@/
=@*�@&{@!��@E�@%@9X@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�`BA�^5A�^5A�bNA�bNA�bNA�bNA�ffA�hsA�hsA�hsA�hsA�jA�l�A�n�A�n�A�p�A�r�A�p�A�n�A�r�A�n�A�n�A�l�A�l�A�jA�ffA�\)A�M�A�G�A�E�A�C�A�?}A�9XA�bA��A�A��A�A�x�A�r�A��A��A���A���A��
A���A�ȴA�VA��-A�n�A�A��-A���A��jA�{A��/A�z�A��A��A�A���A�M�A�Q�A��A���A�\)A�bA���A��
A�VA���A���A�?}A��A��A��A�K�A�/A�$�A��`A�E�A�{A�E�A���A�K�A��A���A�%A�x�A��!A��A���A�1A��A��PA���A���A�l�A��DA�n�A�z�A���A��9A���A��A��/A��/A�1A��!A��RA�
=A��uA�dZA�M�A�r�A�A~��AyK�AxVAsK�An1'Ah��Ah^5Ae��Aa�A_�AY��AX�/AW7LAT�+AS�-AS%ARI�ANȴAI�TAH�AG��AFbNAF�AEp�ACp�AA��A>bNA<�uA<r�A<�A:�uA8�jA7�hA6�RA5�wA4Q�A3+A1�#A1�A0jA/XA.M�A-\)A,��A+��A+�A)VA'�#A&��A&1A%�
A%\)A$�/A$��A$bNA$(�A#�#A#|�A#�A"�jA"5?A!�-A|�AJA�TA�-A33A�A^5A��A�7A�A=qA��A�jA�
A\)A�A��A1AS�A1'A|�A33A�A��A1AQ�AbA�#AhsA=qA1At�A
�A
(�A�A�
AoA�A^5A�TA\)A��A�^A��A=qA�mA�A �j@�K�@�x�@��9@��m@�;d@�n�@�/@��@�1@�"�@�O�@�@�`B@睲@�x�@�!@�l�@܋D@�K�@��#@�V@���@��@�9X@�=q@��;@��@Ѻ^@�C�@�A�@�z�@�|�@ǍP@��y@�5?@�{@�&�@Õ�@�=q@���@��7@��@�A�@���@�1'@�(�@�(�@��
@���@�33@�ȴ@���@���@��m@��F@�t�@��@�$�@�j@��F@�C�@�33@�l�@��@�I�@��\@�r�@��@�dZ@�+@�ff@��w@��
@�`B@��@��H@�{@�v�@�=q@��@��/@�dZ@�o@��+@��\@���@�@�X@��@���@�9X@�X@�=q@��@�X@�p�@�ȴ@���@���@�K�@�t�@�@���@�E�@�{@��@�$�@�@���@�X@�@�hs@�`B@�X@�X@��`@�K�@�@�r�@�C�@�V@�$�@�@�1@�ȴ@�v�@�=q@��@��9@��D@�  @���@�v�@�-@���@�hs@�V@��`@��j@�r�@�I�@�Q�@���@�
=@���@��+@�E�@��^@�`B@�%@��`@��u@�9X@�1'@�(�@�  @��
@���@�C�@��!@�ȴ@��@�^5@�E�@��@��T@�@���@�p�@�O�@��/@��@��D@��u@���@���@��9@���@���@��/@���@��@��@�Q�@� �@�1@��@���@���@�l�@��y@���@���@���@���@��+@�v�@�^5@��@�j@��@��P@���@���@�K�@��@��y@���@�ff@�M�@�M�@��+@�E�@�/@�?}@�G�@�`B@�x�@�&�@� �@�ƨ@�ƨ@��F@�dZ@�
=@�ȴ@���@���@��H@�33@�S�@�S�@�33@��@��@���@��!@��+@�n�@�V@��@��@��T@�`B@�V@��@���@��9@�Q�@���@�S�@�+@��@��@�o@�
=@���@��+@�=q@���@���@��7@�?}@�%@���@�bN@� �@�|�@|j@s�F@kC�@d��@_K�@Xr�@OK�@I��@D�@<�@6E�@/
=@*�@&{@!��@E�@%@9X@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�^B�^B�^B�^B�^B�^B�XB�XB�XB�XB�XB�dB�dB�jB�wB��BĜBĜBŢBŢBƨBǮB��BƨB�qB��B�%B� Bn�B]/Bv�B��B�qB��B��B
=B&�BdZB�hB��B�B��B�wBƨB��B��B��B�LB�B��B�uB}�Bo�BgmBcTBbNB_;BB�BM�BK�Bm�B�DB�bB�bB�\B��B��B��B��B��B��Bp�B}�Bv�Bs�Bo�Bx�Bk�BgmBP�B?}B(�B�BB�B�B�HB��BÖB�LB��B��B�Bv�Be`BH�B+B�B  B
�B
��B
�FB
�JB
k�B
K�B
  B	�
B	��B	��B	s�B	ZB	J�B	49B	!�B	
=B�B�fB�#B��B��BŢB��B�B��B�{B�{B�=B�=B�uB�DBv�Bv�Bu�Bu�Bu�Bv�Bw�Bz�B{�Bz�Bz�Bz�B|�Bv�Bx�B{�Bv�Bt�Bs�B{�Bu�B}�Bp�Bo�Bs�Bq�Bs�Bq�Br�Bq�Br�Bs�Bs�Bu�Br�Bq�Bm�Bk�Bl�Bk�BjBiyBhsBgmBgmBffBe`BdZBcTBaHBaHB`BBaHB_;B^5B^5B^5B^5B]/B\)B[#B[#BYBXBW
BXBR�BS�BR�BVBZBZBR�BO�BO�BM�BS�BYBYBN�B[#BM�BO�BP�BR�BM�BM�BB�BB�BH�BI�BVBC�B=qB;dB@�B=qB;dB>wB8RB49B-B#�B!�B!�B(�B-B2-B5?B9XB9XB8RB7LB8RB+B%�B&�B+B-B,B33B7LB=qB;dB:^B:^B:^B;dB?}BH�BI�BK�BK�BK�BI�BG�BG�BH�BJ�BL�BN�BP�BR�BXBYBZB_;BhsBm�B}�B�B{�B}�B�hB��B��B�PB�bB��B�9B��B�dB��B��B��B�
B�B�)B�)B�;B�B��B��B��B��B	  B��B	{B	�B	{B	{B	�B	%�B	%�B	,B	6FB	5?B	8RB	;dB	?}B	>wB	E�B	F�B	C�B	B�B	W
B	YB	XB	ZB	[#B	XB	P�B	R�B	^5B	O�B	E�B	C�B	F�B	J�B	@�B	A�B	D�B	I�B	G�B	K�B	O�B	P�B	O�B	N�B	O�B	R�B	T�B	T�B	VB	XB	ZB	^5B	_;B	bNB	dZB	ffB	iyB	k�B	m�B	r�B	s�B	t�B	v�B	w�B	x�B	|�B	~�B	� B	~�B	~�B	�B	�+B	�7B	�7B	�=B	�DB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�!B	�!B	�B	�!B	�!B	�-B	�-B	�9B	�FB	�XB	��B	�jB	�FB	�XB	�RB	�qB	�wB	�wB	�}B	��B	��B	��B	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�ZB	�ZB	�TB	�ZB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B
B
oB
�B
$�B
(�B
-B
6FB
;dB
@�B
G�B
N�B
S�B
W
B
]/B
aHB
e`B
k�B
o�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�^B�^B�^B�^B�^B�^B�XB�XB�XB�XB�XB�dB�dB�jB�wB��BĜBĜBŢBŢBƨBȴB��BȴB��B�LB�PB�Br�BcTBw�B��B�qB��B��BVB(�BffB�{B�B�-BÖB��BǮB��B��BƨB�dB�!B��B��B�Bt�BjBe`BdZBcTBF�BS�BL�Bo�B�JB�hB�oB�bB��B��B��B��B��B��Bv�B� By�Bw�Br�B{�Bo�BjBVBH�B-B�B1B��B�B�fB�BȴB�jB��B��B�+B}�Bk�BS�B33B�BB
��B
�HB
ÖB
��B
y�B
`BB
VB	�B	�B	��B	{�B	[#B	O�B	;dB	&�B	�B�B�B�NB��B��BȴB��B�jB��B��B��B�DB�PB��B�hB�B|�Bv�Bw�Bz�B|�B{�B}�B� B� B~�B� B� By�B|�B~�By�Bv�Bu�B}�Bz�B�Bs�Bq�Bt�Bs�Bu�Br�Bs�Br�Bs�Bt�Bu�Bv�Bt�Bs�Bt�Bp�Bm�Bl�Bl�Bk�BjBiyBhsBiyBgmBffBgmBdZBcTBaHBcTBaHBaHBbNBaHB_;B^5B]/B]/B`BBZBYBYB\)BZBVBT�BYB^5B]/BT�BP�BP�BO�BVB[#B\)BQ�B]/BN�BP�BS�BVBP�BO�BD�BC�BJ�BQ�B[#BF�B?}B>wBD�B@�B>wBB�B=qB9XB2-B%�B$�B#�B)�B.B49B9XB=qB;dB;dB<jB>wB2-B'�B&�B-B/B-B5?B:^B@�B<jB;dB;dB<jB<jB?}BH�BI�BL�BL�BL�BJ�BJ�BI�BJ�BK�BM�BP�BR�BW
BZBZBZB_;BhsBm�B�B�B}�B~�B�oB��B��B�PB�VB��B�?B��B�dBB��B��B�B�B�/B�/B�ZB�BB��BȴBƨB��B��B��B	�B	�B	{B	uB	�B	%�B	$�B	,B	7LB	6FB	9XB	<jB	@�B	>wB	F�B	H�B	C�B	B�B	XB	YB	XB	ZB	\)B	[#B	S�B	T�B	`BB	Q�B	F�B	D�B	I�B	L�B	A�B	B�B	F�B	J�B	H�B	L�B	Q�B	Q�B	P�B	O�B	P�B	S�B	VB	VB	W
B	YB	ZB	_;B	`BB	cTB	e`B	gmB	jB	l�B	n�B	s�B	t�B	u�B	v�B	w�B	y�B	}�B	� B	�B	� B	~�B	�B	�1B	�=B	�=B	�DB	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�!B	�'B	�'B	�B	�!B	�!B	�-B	�3B	�9B	�LB	�dB	B	�wB	�FB	�XB	�RB	�wB	�}B	�}B	��B	��B	��B	��B	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�;B	�;B	�;B	�HB	�BB	�HB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�ZB	�`B	�ZB	�mB	�yB	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B
B
uB
�B
%�B
(�B
-B
6FB
;dB
@�B
G�B
O�B
S�B
XB
^5B
aHB
e`B
k�B
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<T��<49X<e`B<��
<e`B<#�
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
<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452082012011014520820120110145208  AO  ARGQ                                                                        20111130143943  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143943  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145208  IP                  G�O�G�O�G�O�                