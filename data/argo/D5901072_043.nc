CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               +A   AO  20111130143930  20190522121828  1728_5048_043                   2C  D   APEX                            2142                            040306                          846 @ԕ��V��1   @ԕ�K� 	@6KC��%�cB��`A�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk�fDlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dys3D��3D�@ D�vfD���D�fD�0 D���D��3D���D�&fD�l�D��3D��3D��Dڌ�D��D���D�6fD�Y�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B}��B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cֳ3C�� C�� C�� C���C���C���C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D�fD` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.Y�D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ�fD[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh�fDi` Di� Dj` Dj� DkffDk�fDl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� DyS3D��3D�0 D�ffD���D��fD�  D�y�D��3D���D�fD�\�Dǳ3D��3D��D�|�D���D���D�&fD�I�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��#A��#A��#A��A��A��#A��#A��/A��;A��;A��/A��HA��HA��TA��`A��`A��`A��TA��HA��TA��`A��mA��mA��mA��yA��mA��HA���Aϙ�AΝ�A�n�A�t�A�  A¸RA��A�bNA�`BA�A��7A�=qA�ffA��+A��#A�z�A��`A��A���A��7A�=qA��A�bA�A�`BA��A��yA���A�{A�A�A�$�A��A��A�JA�1'A���A��wA�jA�A��FA�r�A�VA�A��wA��+A�1A���A�\)A��/A�l�A���A��9A�`BA�-A�ƨA���A�p�A��A�|�A���A���A�|�A� �A�~�A��wA�$�A�Q�A��jA�^5A��jA�ƨA��A�
=A�A��
A� �A��A���A��PA�v�A���A�~�A���A���A���A�AG�A~I�Az��Av^5Ap��AlbNAj5?Ai�;Ai�AgXAd1AbbA`��A^�HA[7LAW�#AU�#AS�PAR�9AR�\AQdZAO�PAM�-AIp�AF�AC��A@(�A>jA:JA7�A6-A5��A5x�A4ȴA3p�A2I�A1x�A1A09XA/C�A.�A.5?A-��A,�A+��A+��A+x�A+
=A*�RA*~�A*A)�^A(VA%�PA$�\A$-A#ƨA#hsA#�A"��A!�;A!S�A �yAoA�-A7LA�`A��A�
AbA�9Ap�A��AVA�#AdZAn�AO�AbNA�`A�AQ�A%A�hA
�jA	+AI�A�hA"�A��A=qA�A�PA�A��AZAJA|�A��AA��A7LA �!@��
@��+@���@�ff@���@��@���@�@�?}@��@�R@�{@�n�@���@���@�P@���@�dZ@��@�E�@ܴ9@ۥ�@ّh@�bN@�(�@׶F@�t�@��@և+@�$�@Չ7@ԣ�@Ӯ@ҏ\@��@ѩ�@љ�@�X@��m@Ώ\@́@�Q�@���@�n�@�p�@�1'@�+@��@�%@�(�@�o@�^5@�J@�O�@��D@�@���@���@��R@��@���@�j@�(�@���@��w@�;d@��H@�-@�x�@��u@��P@��R@�ff@��@��^@�7L@��u@�1@�S�@���@�E�@��@��@��-@�`B@��`@�I�@���@���@���@�`B@�V@��u@�  @�ƨ@���@�\)@�
=@��!@��T@���@�j@�j@�r�@�bN@��@�  @���@��-@��@�p�@�p�@�p�@�`B@�X@�O�@�?}@�Ĝ@�bN@�Z@�A�@���@��@�+@���@��+@�5?@�@��#@�@���@�G�@���@��D@��@�|�@�A�@�?}@���@�@�@��@�G�@�%@�(�@�+@�
=@��@�v�@�`B@�%@��9@�Z@�1'@� �@��@�K�@�ȴ@���@�~�@�V@�-@�@��@��@��T@���@���@���@���@�@��^@���@��@�r�@�9X@��@�1@��
@���@�K�@��@�ff@�E�@��@���@���@���@�@��h@�/@��@��`@��@�r�@�bN@�A�@�b@��;@�l�@�@��@��R@��\@�^5@�V@�E�@�-@�@��@��@��T@���@��@�`B@�?}@��@���@���@���@�A�@�  @�ƨ@�|�@�K�@�+@�o@�
=@�@��y@��!@�^5@�=q@�-@�{@��@���@�hs@�G�@�&�@�V@��@�Ĝ@���@�bN@� �@��;@�|�@�;d@��@�@��H@���@�ff@�5?@�@���@�x�@�hs@�G�@��/@�r�@�  @��@�t�@�dZ@�\)@�S�@�;d@��@���@��!@��\@�V@�=q@��@|z�@tj@k��@b��@X��@Q7L@I��@E�@@Q�@;t�@5�@0�9@,�@'�P@"=q@�R@�@��@�9@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��#A��#A��#A��#A��A��A��#A��#A��/A��;A��;A��/A��HA��HA��TA��`A��`A��`A��TA��HA��TA��`A��mA��mA��mA��yA��mA��HA���Aϙ�AΝ�A�n�A�t�A�  A¸RA��A�bNA�`BA�A��7A�=qA�ffA��+A��#A�z�A��`A��A���A��7A�=qA��A�bA�A�`BA��A��yA���A�{A�A�A�$�A��A��A�JA�1'A���A��wA�jA�A��FA�r�A�VA�A��wA��+A�1A���A�\)A��/A�l�A���A��9A�`BA�-A�ƨA���A�p�A��A�|�A���A���A�|�A� �A�~�A��wA�$�A�Q�A��jA�^5A��jA�ƨA��A�
=A�A��
A� �A��A���A��PA�v�A���A�~�A���A���A���A�AG�A~I�Az��Av^5Ap��AlbNAj5?Ai�;Ai�AgXAd1AbbA`��A^�HA[7LAW�#AU�#AS�PAR�9AR�\AQdZAO�PAM�-AIp�AF�AC��A@(�A>jA:JA7�A6-A5��A5x�A4ȴA3p�A2I�A1x�A1A09XA/C�A.�A.5?A-��A,�A+��A+��A+x�A+
=A*�RA*~�A*A)�^A(VA%�PA$�\A$-A#ƨA#hsA#�A"��A!�;A!S�A �yAoA�-A7LA�`A��A�
AbA�9Ap�A��AVA�#AdZAn�AO�AbNA�`A�AQ�A%A�hA
�jA	+AI�A�hA"�A��A=qA�A�PA�A��AZAJA|�A��AA��A7LA �!@��
@��+@���@�ff@���@��@���@�@�?}@��@�R@�{@�n�@���@���@�P@���@�dZ@��@�E�@ܴ9@ۥ�@ّh@�bN@�(�@׶F@�t�@��@և+@�$�@Չ7@ԣ�@Ӯ@ҏ\@��@ѩ�@љ�@�X@��m@Ώ\@́@�Q�@���@�n�@�p�@�1'@�+@��@�%@�(�@�o@�^5@�J@�O�@��D@�@���@���@��R@��@���@�j@�(�@���@��w@�;d@��H@�-@�x�@��u@��P@��R@�ff@��@��^@�7L@��u@�1@�S�@���@�E�@��@��@��-@�`B@��`@�I�@���@���@���@�`B@�V@��u@�  @�ƨ@���@�\)@�
=@��!@��T@���@�j@�j@�r�@�bN@��@�  @���@��-@��@�p�@�p�@�p�@�`B@�X@�O�@�?}@�Ĝ@�bN@�Z@�A�@���@��@�+@���@��+@�5?@�@��#@�@���@�G�@���@��D@��@�|�@�A�@�?}@���@�@�@��@�G�@�%@�(�@�+@�
=@��@�v�@�`B@�%@��9@�Z@�1'@� �@��@�K�@�ȴ@���@�~�@�V@�-@�@��@��@��T@���@���@���@���@�@��^@���@��@�r�@�9X@��@�1@��
@���@�K�@��@�ff@�E�@��@���@���@���@�@��h@�/@��@��`@��@�r�@�bN@�A�@�b@��;@�l�@�@��@��R@��\@�^5@�V@�E�@�-@�@��@��@��T@���@��@�`B@�?}@��@���@���@���@�A�@�  @�ƨ@�|�@�K�@�+@�o@�
=@�@��y@��!@�^5@�=q@�-@�{@��@���@�hs@�G�@�&�@�V@��@�Ĝ@���@�bN@� �@��;@�|�@�;d@��@�@��H@���@�ff@�5?@�@���@�x�@�hs@�G�@��/@�r�@�  @��@�t�@�dZ@�\)@�S�@�;d@��@���@��!@��\@�V@�=q@��@|z�@tj@k��@b��@X��@Q7L@I��@E�@@Q�@;t�@5�@0�9@,�@'�P@"=q@�R@�@��@�9@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBĜBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBƨBƨBƨBŢBŢBŢBŢBŢBƨBŢBƨBƨBƨBƨBƨBƨBŢBĜB�}B��Bv�B�1B�7B�\B}�Br�B{�B�=B�VBbNB�{B�B��B��BǮB�;B�B  B,B2-B,B#�B33B6FB>wB[#Bp�Bz�B}�B� B�B�PB�PB�=B�PB�DB�DB�DB�7B�+B�+B�7B�+B�B�1B�B�B�B��B�B�B��B��B��B�VB�Bv�BG�B&�B �B�B�BB��B�;B�B��BB�RB��Br�BP�BG�BS�BG�B
=B
��B
��B
� B
m�B
>wB
�B	��B	ÖB	ɺB	�sB	��B	��B	m�B	A�B	7LB	49B	0!B	6FB	&�B	hB	1B	B�B�)B�jBƨB�B	
=B	uB	B�B��B��B�+Bn�BffBhsBw�B� B|�B� B�B~�B{�B{�B{�Bz�By�Bu�Bu�Bv�Bu�Bt�Bt�Bv�Bw�By�B|�B~�B�B|�Bt�Bq�Bp�Bp�Bp�Bo�Bp�Bv�Br�Bt�BiyBe`BgmBffBe`BhsBo�Be`B_;B^5B]/B]/B]/B^5B^5B\)BcTBYB\)BQ�BQ�BO�BP�BQ�BN�BM�BL�BL�BL�BL�BM�BM�BL�BK�BJ�BL�BJ�BJ�BM�BI�BJ�BI�BA�BJ�BB�BA�BB�BG�BC�BN�BG�BA�BO�BC�B:^B8RB2-B2-B0!B33B8RB8RB9XB1'B1'B0!B0!B/B/B/B/B/B0!B/B/B0!B1'B2-B33B33B5?B6FB5?B5?B5?B5?B7LB7LB5?B5?B8RB;dB=qB=qB?}BC�BK�BI�BL�BN�BS�BS�BT�BVBW
BXBXBZB^5BbNBffBffBgmBn�Bo�Bt�Bu�Bw�Bz�B~�B�B�B�B�B�%B�=B�DB�VB��B��B��B��B��B��B��B��B�B�B�!B�?B�^BÖBǮBȴBɺB��B��B��B��B��B��B��B��B��B�B�B�
B�B�)B�)B�5B�NB�`B�fB�mB�B�B�B��B��B��B��B	B	%B	JB	bB	�B	+B	2-B	7LB	@�B	D�B	E�B	I�B	N�B	Q�B	S�B	W
B	\)B	ZB	YB	YB	XB	YB	YB	[#B	bNB	hsB	jB	l�B	o�B	q�B	s�B	t�B	t�B	u�B	v�B	w�B	w�B	w�B	x�B	x�B	z�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�=B	�\B	�bB	�JB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�-B	�3B	�?B	�LB	�RB	�^B	�qB	�}B	�}B	��B	��B	B	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�#B	�/B	�5B	�)B	�)B	�)B	�#B	�)B	�;B	�;B	�5B	�5B	�;B	�NB	�NB	�NB	�ZB	�sB	�sB	�sB	�yB	�B	��B
B
\B
�B
%�B
-B
49B
:^B
?}B
D�B
I�B
N�B
S�B
ZB
aHB
e`B
jB
p�B
t�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BĜBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBƨBƨBƨBŢBŢBŢBŢBŢBƨBŢBƨBƨBƨBƨBƨBƨBŢBŢBBĜBz�B�uB�\B�{B�Bu�B|�B�JB��BbNB��B�JB��B�B��B�BB�BB-B6FB0!B&�B49B7LB?}BaHBt�B{�B~�B�B�+B�hB�bB�VB�\B�PB�PB�hB�DB�+B�7B�=B�=B�1B�=B�'B�!B�'B��B�B�B�B��B��B�oB�PB~�BO�B'�B"�B"�B�B+B��B�NB�B��BƨB�jB��B{�BW
BK�B[#BQ�B�B
�5B
�B
�%B
v�B
J�B
&�B	�B	ŢB	��B	�B	�#B	�B	z�B	H�B	8RB	6FB	7LB	@�B	-B	�B	JB	1B��B�BB��BȴB�B	PB	�B	
=B��B��B�!B�hBt�Bt�Bo�B}�B�B}�B�B�%B�B~�B}�B� B~�B|�Bw�Bw�Bz�By�Bv�Bu�Bx�By�Bz�B~�B�B�=B�+Bx�Bs�Br�Br�Br�Bq�Bs�Bx�Bt�By�Bm�BgmBhsBgmBgmBl�Bs�BhsBaHB_;B_;B_;B`BBbNBaHBaHBffB^5B`BBW
BT�BT�BS�BT�BP�BO�BN�BM�BN�BO�BN�BO�BN�BM�BM�BO�BL�BL�BO�BL�BM�BL�BD�BL�BE�BB�BD�BH�BD�BQ�BM�BF�BS�BF�B=qB=qB33B33B1'B6FB:^B;dB;dB2-B2-B1'B1'B0!B0!B0!B1'B1'B2-B0!B0!B1'B2-B5?B6FB5?B7LB9XB6FB7LB8RB7LB9XB9XB7LB7LB8RB<jB?}B?}BB�BG�BM�BK�BN�BP�BT�BT�BVBW
BXBYBZB\)B`BBdZBhsBgmBhsBo�Bp�Bu�Bv�Bx�B{�B� B�B�B�B�B�+B�DB�JB�bB��B��B��B��B��B��B��B�B�B�B�-B�LB�dBÖBǮBɺB��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�)B�/B�;B�TB�fB�mB�sB�B�B��B��B��B��B	  B	B	+B	PB	\B	�B	+B	2-B	7LB	A�B	E�B	F�B	K�B	P�B	R�B	T�B	XB	^5B	[#B	ZB	ZB	YB	YB	ZB	\)B	cTB	iyB	k�B	m�B	p�B	r�B	s�B	t�B	t�B	u�B	v�B	w�B	w�B	w�B	x�B	y�B	{�B	�B	�%B	�%B	�%B	�1B	�=B	�DB	�DB	�bB	�bB	�PB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�'B	�-B	�3B	�FB	�RB	�XB	�dB	�wB	��B	��B	��B	��B	ÖB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�5B	�)B	�5B	�;B	�)B	�)B	�/B	�)B	�/B	�BB	�BB	�5B	�5B	�;B	�NB	�NB	�TB	�`B	�yB	�yB	�sB	�yB	�B	��B
B
\B
�B
&�B
-B
5?B
:^B
?}B
D�B
J�B
O�B
S�B
ZB
aHB
ffB
k�B
p�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<49X<#�
<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<T��<�o<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452082012011014520820120110145208  AO  ARGQ                                                                        20111130143930  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143930  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145208  IP                  G�O�G�O�G�O�                