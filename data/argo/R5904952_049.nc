CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ST   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  m8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  vx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190516  20181005190516  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               1A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׻��v<1   @׻�8㠖@1|j~��#�c���Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      1A   A   A   @�  @�  A   A   A@  A`  A�  A���A���A���A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C�fC"  C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  Dy�D  D�fD  D� D  Dy�D��D� DfD�fDfD� D  D� D	fD	� D	��D
� D  D� DfD�fD  D�fD  D� D  D� D  D� D  D� D  Dy�D  D� D��Dy�D��Dy�D  D� D  D� D��Dy�D��D� D  D�fDfD�fDfD�fDfD�fDfD�fDfD� D   D �fD!  D!� D"fD"� D"��D#� D$  D$� D%  D%�fD&  D&� D'  D'y�D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.�fD/  D/y�D0  D0� D1  D1� D2fD2� D2��D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D9��D:� D;fD;�fD<  D<� D=  D=� D>fD>� D>��D?� D@  D@y�D@��DAy�DA��DBy�DB��DC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI�fDJ  DJy�DJ��DK� DLfDL� DM  DM� DN  DNy�DO  DO�fDPfDP� D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh�fDi  Di� Dj  Djy�Dk  Dky�Dl  Dl� Dm  Dmy�Dn  Dn�fDofDo�fDp  Dp� Dp��Dq� Dr  Dr�fDs  Ds� DtfDt� Du  Du� Du��Dvy�Dw  Dw` Dyb�D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�\A"�\AB�\Ab�\A�G�A�{A�{A�{A�{A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C\C(�C(�C(�C(�C(�C \C"(�C$(�C&\C(\C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�CtB�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�!HC�!HC�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�!HC�!HC�{C�{C��C�{C�!HC�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C��C��C�{C�{C��C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{D 
=D �=D
=D��D
=D��D
=D�=D
=D��D�D�=D�D��D�D�=D
=D�=D	�D	�=D
�D
�=D
=D�=D�D��D
=D��D
=D�=D
=D�=D
=D�=D
=D�=D
=D��D
=D�=D�D��D�D��D
=D�=D
=D�=D�D��D�D�=D
=D��D�D��D�D��D�D��D�D��D�D�=D 
=D ��D!
=D!�=D"�D"�=D#�D#�=D$
=D$�=D%
=D%��D&
=D&�=D'
=D'��D(
=D(��D)
=D)�=D*
=D*�=D+
=D+�=D,
=D,�=D-
=D-��D.
=D.��D/
=D/��D0
=D0�=D1
=D1�=D2�D2�=D3�D3��D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9�D9�=D:�D:�=D;�D;��D<
=D<�=D=
=D=�=D>�D>�=D?�D?�=D@
=D@��DA�DA��DB�DB��DC�DC�=DD
=DD�=DE
=DE�=DF
=DF�=DG�DG��DH
=DH�=DI
=DI��DJ
=DJ��DK�DK�=DL�DL�=DM
=DM�=DN
=DN��DO
=DO��DP�DP�=D\�=D]
=D]�=D^
=D^��D_
=D_�=D`
=D`�=Da
=Da��Db
=Db�=Dc
=Dc�=Dd
=Dd�=De
=De�=Df
=Df�=Dg
=Dg��Dh
=Dh��Di
=Di�=Dj
=Dj��Dk
=Dk��Dl
=Dl�=Dm
=Dm��Dn
=Dn��Do�Do��Dp
=Dp�=Dq�Dq�=Dr
=Dr��Ds
=Ds�=Dt�Dt�=Du
=Du�=Dv�Dv��Dw
=Dwj=Dyl�D�;�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��HA��;A��HA��;A��;A��;A��A���Aײ-AׅA�dZA�E�A�7LA�$�A���A��A��yA��mA��HA��A�ƨAօA��A��#A�A�t�A�bA�VA��`A��#A��A�ȴAԼjAԮAԙ�A�?}Aд9A�z�A�t�A��A�v�A�?}A˓uA�
=AʍPA�&�A�E�Aȡ�A�+Aǝ�A�ĜAğ�A��AÑhA�ffA�O�A�dZA�/A�JA��FA�JA�x�A�JA�%A�XA�jA�p�A���A�VA��A��A�Q�A�(�A�+A�$�A�ZA�(�A��A�r�A�5?A���A��A�&�A���A�ƨA�5?A���A�^5A�VA�S�A�C�A�ffA�hsA���A���A��/A���A�~�A�1A�$�A���A�+A���A�~�A�M�A�ȴA�S�A��A���A��A��;A��PA�(�Ax�\AtM�As\)Ar�+Aj�jAiS�AhA�Ae�#A`(�A\z�A[|�AY�hAW�hAT��AS�ASp�AR�yANȴAL��AK7LAJ�AJ��AI�^AH5?AGVAF��AD�ACVA>��A9�mA6�A3C�A21'A0��A/K�A-�mA-
=A,1A)l�A(A�A'A%�
A$�9A"��A!�A!;dAA��A|�A�`A��A
=A��A�A&�A��A(�A&�A=qA�Av�AXAffA��A"�A�HAM�A=qAhsA�AA�A
�A
�\A
A�A	�TA�AQ�A��Ar�A��AQ�A�7A
=A�A��A�AXA?}AS�A �RA ��AVA 1'@�-@�Z@�dZ@���@���@�X@��y@��@�I�@�V@�D@�\)@�$�@�o@���@���@���@�bN@߾w@�t�@�
=@�n�@�hs@�"�@��#@ى7@�G�@�/@��/@׾w@�33@��@ָR@���@�$�@ԛ�@�@�p�@д9@Ͼw@ϕ�@�t�@�K�@��@�~�@��@�p�@̬@�9X@�K�@�dZ@�l�@�K�@��
@�1'@�;d@�~�@ɡ�@�Ĝ@�I�@�b@��@ǥ�@���@�\)@��H@ēu@��@��@�l�@�+@�@���@�b@��;@�ȴ@�$�@��@���@��@�(�@�1'@���@�M�@�-@�{@��@�p�@���@�&�@�+@�33@�C�@�;d@���@�n�@�ff@�%@�z�@�hs@�33@���@��@�K�@��H@��!@���@�v�@�ff@��@��@��9@���@��u@�r�@�I�@���@�t�@�;d@�+@�+@�+@���@���@�X@��@��/@���@�r�@�Q�@�1'@��m@�|�@�C�@�v�@�J@���@�X@�bN@�K�@��H@��R@��\@��\@��+@�n�@���@�G�@��j@�Ĝ@�G�@�O�@��u@���@�K�@��y@���@���@�~�@��+@�ff@��@�1'@�j@�A�@���@��+@��+@��@���@�O�@��@�r�@�r�@��@�z�@�  @���@�;d@�+@�C�@�K�@�S�@�|�@��P@��P@���@���@�X@�?}@�7L@���@� �@�|�@���@�ff@�^5@�V@��@��-@��7@��@�O�@�X@�G�@�&�@�r�@�t�@���@�`B@�O�@�&�@���@���@��j@��u@�r�@�Z@�Q�@�  @�@��!@�~�@�5?@��T@��7@���@���@�bN@��F@�C�@�@��H@��\@�@��T@���@�7L@��@�bN@��@��@�"�@�~�@��@���@��@��@�9X@�  @���@�+@��y@��!@��\@�~�@��+@�M�@�{@��T@���@���@�p�@�X@�X�@��F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��HA��;A��HA��;A��;A��;A��A���Aײ-AׅA�dZA�E�A�7LA�$�A���A��A��yA��mA��HA��A�ƨAօA��A��#A�A�t�A�bA�VA��`A��#A��A�ȴAԼjAԮAԙ�A�?}Aд9A�z�A�t�A��A�v�A�?}A˓uA�
=AʍPA�&�A�E�Aȡ�A�+Aǝ�A�ĜAğ�A��AÑhA�ffA�O�A�dZA�/A�JA��FA�JA�x�A�JA�%A�XA�jA�p�A���A�VA��A��A�Q�A�(�A�+A�$�A�ZA�(�A��A�r�A�5?A���A��A�&�A���A�ƨA�5?A���A�^5A�VA�S�A�C�A�ffA�hsA���A���A��/A���A�~�A�1A�$�A���A�+A���A�~�A�M�A�ȴA�S�A��A���A��A��;A��PA�(�Ax�\AtM�As\)Ar�+Aj�jAiS�AhA�Ae�#A`(�A\z�A[|�AY�hAW�hAT��AS�ASp�AR�yANȴAL��AK7LAJ�AJ��AI�^AH5?AGVAF��AD�ACVA>��A9�mA6�A3C�A21'A0��A/K�A-�mA-
=A,1A)l�A(A�A'A%�
A$�9A"��A!�A!;dAA��A|�A�`A��A
=A��A�A&�A��A(�A&�A=qA�Av�AXAffA��A"�A�HAM�A=qAhsA�AA�A
�A
�\A
A�A	�TA�AQ�A��Ar�A��AQ�A�7A
=A�A��A�AXA?}AS�A �RA ��AVA 1'@�-@�Z@�dZ@���@���@�X@��y@��@�I�@�V@�D@�\)@�$�@�o@���@���@���@�bN@߾w@�t�@�
=@�n�@�hs@�"�@��#@ى7@�G�@�/@��/@׾w@�33@��@ָR@���@�$�@ԛ�@�@�p�@д9@Ͼw@ϕ�@�t�@�K�@��@�~�@��@�p�@̬@�9X@�K�@�dZ@�l�@�K�@��
@�1'@�;d@�~�@ɡ�@�Ĝ@�I�@�b@��@ǥ�@���@�\)@��H@ēu@��@��@�l�@�+@�@���@�b@��;@�ȴ@�$�@��@���@��@�(�@�1'@���@�M�@�-@�{@��@�p�@���@�&�@�+@�33@�C�@�;d@���@�n�@�ff@�%@�z�@�hs@�33@���@��@�K�@��H@��!@���@�v�@�ff@��@��@��9@���@��u@�r�@�I�@���@�t�@�;d@�+@�+@�+@���@���@�X@��@��/@���@�r�@�Q�@�1'@��m@�|�@�C�@�v�@�J@���@�X@�bN@�K�@��H@��R@��\@��\@��+@�n�@���@�G�@��j@�Ĝ@�G�@�O�@��u@���@�K�@��y@���@���@�~�@��+@�ff@��@�1'@�j@�A�@���@��+@��+@��@���@�O�@��@�r�@�r�@��@�z�@�  @���@�;d@�+@�C�@�K�@�S�@�|�@��P@��P@���@���@�X@�?}@�7L@���@� �@�|�@���@�ff@�^5@�V@��@��-@��7@��@�O�@�X@�G�@�&�@�r�@�t�@���@�`B@�O�@�&�@���@���@��j@��u@�r�@�Z@�Q�@�  @�@��!@�~�@�5?@��T@��7@���@���@�bN@��F@�C�@�@��H@��\@�@��T@���@�7L@��@�bN@��@��@�"�@�~�@��@���@��@��@�9X@�  @���@�+@��y@��!@��\@�~�@��+@�M�@�{@��T@���@���@�p�@�X@�X�@��F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
9XB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
9XB
9XB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
A�B
T�B
dZB
k�B
{�B
�oB
��B
�B
�?B
�jB
B
ǮB
��B
��B
��B
ÖB
ƨB
��B
�5B
��BVBPB�B2-BA�BYBr�B|�B�VBÖB�B�yB�B��B��B��B��BPB�B#�B'�B,BB�BE�BC�BD�BM�Bt�B��B�FB�LB�'B��B��B��B�bB�%B�B~�B{�Bu�BjB]/BK�B7LB&�B �B�B�B�BoB��B�B��B�B�{B��B��B�+Bl�BC�B$�B
��B
�B
n�B
k�B
jB
gmB
dZB
O�B
8RB
uB	��B	�-B	�-B	�B	�B	v�B	m�B	\)B	A�B	33B	/B	'�B	 �B	�B	uB	hB	PB��B��B�B�B�B�sB�NB�5B�#B�B��BÖB�RB�'B�-B�9B�?B�?B�?B�LB�FB�3B�?B�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�B�FB�FB�9B�?B�?B�LB�XBÖB�RB�RB�jB��B�}BÖBŢBĜBĜBÖBBBƨB��BɺB��B��B��BƨBĜBƨBȴBƨB��BɺB��B��BÖBŢBǮBŢBƨBǮBǮBǮBǮBȴBɺB��B��BȴBɺB��B��B��B�
B�#B�BB�`B�B�B��B�B�B�yB�mB�B��B��B��B��B��B��B��B��B��B��B	B	%B	PB	{B	�B	�B	 �B	!�B	�B	�B	�B	 �B	!�B	!�B	'�B	(�B	(�B	0!B	1'B	0!B	1'B	0!B	/B	.B	0!B	1'B	33B	5?B	5?B	5?B	7LB	A�B	D�B	C�B	B�B	C�B	D�B	H�B	K�B	O�B	XB	ffB	gmB	gmB	gmB	gmB	jB	n�B	p�B	q�B	y�B	�+B	�DB	�DB	�VB	�bB	�bB	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�?B	�FB	�LB	�?B	�9B	�9B	�?B	�RB	�XB	�^B	�dB	�dB	�jB	��B	B	��B	��B	ĜB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
%B
+B
+B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
1B
+B
+B
%B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
+B
+B
1B
	7B
	7B
	�B
	22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
9XB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
9XB
9XB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
A�B
T�B
dZB
k�B
{�B
�oB
��B
�B
�?B
�jB
B
ǮB
��B
��B
��B
ÖB
ƨB
��B
�5B
��BVBPB�B2-BA�BYBr�B|�B�VBÖB�B�yB�B��B��B��B��BPB�B#�B'�B,BB�BE�BC�BD�BM�Bt�B��B�FB�LB�'B��B��B��B�bB�%B�B~�B{�Bu�BjB]/BK�B7LB&�B �B�B�B�BoB��B�B��B�B�{B��B��B�+Bl�BC�B$�B
��B
�B
n�B
k�B
jB
gmB
dZB
O�B
8RB
uB	��B	�-B	�-B	�B	�B	v�B	m�B	\)B	A�B	33B	/B	'�B	 �B	�B	uB	hB	PB��B��B�B�B�B�sB�NB�5B�#B�B��BÖB�RB�'B�-B�9B�?B�?B�?B�LB�FB�3B�?B�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�B�FB�FB�9B�?B�?B�LB�XBÖB�RB�RB�jB��B�}BÖBŢBĜBĜBÖBBBƨB��BɺB��B��B��BƨBĜBƨBȴBƨB��BɺB��B��BÖBŢBǮBŢBƨBǮBǮBǮBǮBȴBɺB��B��BȴBɺB��B��B��B�
B�#B�BB�`B�B�B��B�B�B�yB�mB�B��B��B��B��B��B��B��B��B��B��B	B	%B	PB	{B	�B	�B	 �B	!�B	�B	�B	�B	 �B	!�B	!�B	'�B	(�B	(�B	0!B	1'B	0!B	1'B	0!B	/B	.B	0!B	1'B	33B	5?B	5?B	5?B	7LB	A�B	D�B	C�B	B�B	C�B	D�B	H�B	K�B	O�B	XB	ffB	gmB	gmB	gmB	gmB	jB	n�B	p�B	q�B	y�B	�+B	�DB	�DB	�VB	�bB	�bB	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�?B	�FB	�LB	�?B	�9B	�9B	�?B	�RB	�XB	�^B	�dB	�dB	�jB	��B	B	��B	��B	ĜB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
%B
+B
+B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
1B
+B
+B
%B
B
B
%B
%B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
+B
+B
1B
	7B
	7B
	�B
	22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190516                              AO  ARCAADJP                                                                    20181005190516    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190516  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190516  QCF$                G�O�G�O�G�O�8000            