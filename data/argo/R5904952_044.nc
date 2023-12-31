CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:15Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190515  20181005190515  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ,A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׺d�W��1   @׺eq�p@1��\(���c�
=p��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ,A   B   B   @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A���B ffBffBffBffB ffB(  B/��B8  B?��BG��BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B���C   C  C  C�fC�fC
  C�C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D��D� D  Dy�D��D� D	fD	� D
  D
� D  D� D  D� D  D� D��D� DfD�fD  D� D  Dy�D  D�fD  D� DfD� D  Dy�D��D� D  D� D��D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(y�D(��D)� D*  D*y�D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3fD3� D4  D4� D5  D5� D6fD6y�D6��D7� D8  D8� D9fD9� D:  D:� D;  D;� D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DG��DHy�DI  DI�fDJfDJ�fDKfDK� DK��DLy�DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DV��DW� DX  DXy�DY  DY�fDZfDZ�fD[fD[� D[��D\y�D\��D]� D^  D^� D^��D_� D`  D`� Da  Day�Db  Db� Db��Dc�fDd  Dd� DefDe� Df  Df�fDgfDg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do�fDp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dwy�Dw� Dy�\D�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @C�@��@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�z�A�G�A�G�A�{B
=B	
=B
=B
=B!
=B(��B0=qB8��B@=qBH=qBP=qBX=qB`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�B�Q�B�B�Q�B�Q�B�Q�B��C (�C(�C(�C\C\C
(�CB�CB�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN\CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj\Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�!HC�{C�{C�{C��C�{C�{C��C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C��C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C��C�{C�!HC�!HC�{C��C�{C�{C�{C�!HC�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�!HC�{C��C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{D 
=D �=D
=D�=D
=D��D
=D�=D
=D�=D
=D�=D�D�=D
=D��D�D�=D	�D	�=D

=D
�=D
=D�=D
=D�=D
=D�=D�D�=D�D��D
=D�=D
=D��D
=D��D
=D�=D�D�=D
=D��D�D�=D
=D�=D�D�=D
=D�=D�D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D 
=D �=D!
=D!�=D"�D"�=D#�D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'�=D(�D(��D)�D)�=D*
=D*��D+
=D+��D,�D,�=D-
=D-�=D.
=D.�=D/
=D/�=D0
=D0�=D1
=D1�=D2�D2�=D3�D3�=D4
=D4�=D5
=D5�=D6�D6��D7�D7�=D8
=D8�=D9�D9�=D:
=D:�=D;
=D;�=D<�D<��D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC
=DC�=DD
=DD�=DE�DE�=DF
=DF�=DG
=DG�=DH�DH��DI
=DI��DJ�DJ��DK�DK�=DL�DL��DM
=DM�=DN�DN�=DO
=DO�=DP
=DP�=DQ
=DQ�=DR
=DR�=DS
=DS�=DT
=DT��DU
=DU�=DV
=DV�=DW�DW�=DX
=DX��DY
=DY��DZ�DZ��D[�D[�=D\�D\��D]�D]�=D^
=D^�=D_�D_�=D`
=D`�=Da
=Da��Db
=Db�=Dc�Dc��Dd
=Dd�=De�De�=Df
=Df��Dg�Dg�=Dh
=Dh�=Di�Di�=Dj
=Dj�=Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn��Do
=Do��Dp
=Dp�=Dq
=Dq��Dr
=Dr�=Ds
=Ds�=Dt
=Dt��Du
=Du�=Dv
=Dv�=Dw
=Dw��Dw�=Dy��D�B>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aա�Aգ�Aհ!Aմ9AլAհ!Aղ-Aղ-Aղ-AնFA���A���AվwA�ƨA���A���A���A���A���A�ƨA���Aպ^Aպ^AծAե�A՗�A�x�A�jA�dZA�S�A�G�A�5?A�JA��#A�&�A��;A���Aң�AҋDA�-A���A��yA�-A�~�A��;A�G�A͡�A�=qA�VA��`A���A�C�A�p�Aʇ+A�~�A�JA���A�dZA�+A��/Aź^A�ZA���A�M�A��DA�p�A��HA�p�A�"�A��jA���A�  A���A���A��A���A��A��RA�^5A�5?A�oA��yA�ZA�1'A�A�ffA�v�A�n�A��\A���A���A��^A�x�A�5?A��A�VA���A� �A�+A�?}A� �A��A�x�A�z�A�
=A���A��A��A��/A�%A��A�VA���A��A��-A�M�A�7LA�
=A~�DAz�+Au7LApv�Akx�AhAg/Afn�Af(�Ad��AbM�A_�wA^1'A[�mAW��AQ�AP�AO�#AM��AK�-AJ�AIl�AHJAF�AD �ABr�AA�
AAS�A@��A?�wA<�A:�A9C�A7O�A6�A3��A01'A/
=A-�TA,�A*�A'C�A%�A$��A#x�A!\)A VA�;AG�A��AM�A��A=qAK�Ax�A�AA�A��AZA�TA�A�AA�AAx�Ar�A��AG�A�HA  A�A|�A�PA
  A	&�A	VA��A	�A
ZAp�A7LA	�A	VA$�A�A�TAA�Ar�A/AK�A�AXA ȴ@�\)@���@�=q@�M�@�h@ߍP@���@�5?@�O�@�j@��m@�o@���@��m@ׅ@ו�@�\)@�v�@��@ԣ�@Ӿw@҇+@�E�@��@�V@�\)@Χ�@̴9@�@�^5@��@�?}@���@ȃ@ȣ�@�E�@�~�@��@˝�@�ƨ@˅@�t�@�\)@�n�@�/@Ǯ@��@Ƈ+@�n�@�5?@�M�@��@ũ�@ļj@��
@�o@��@�^5@���@�hs@���@���@�Z@��@��@�ƨ@�@�V@�^5@�ff@��@�hs@��@�x�@��7@�p�@�/@��D@�S�@��H@�n�@��@�hs@�7L@�Q�@�K�@�n�@�v�@���@�v�@�E�@��@�O�@���@���@��D@�9X@���@���@�o@��+@�Z@��F@�C�@���@���@��@���@���@�`B@�/@��@���@���@��@��D@�ƨ@�t�@�33@��+@���@�@�E�@�M�@�=q@��@�{@��T@���@�@��^@�@�V@���@�^5@�E�@�-@���@��-@�/@�j@�I�@�1'@��;@���@��@�C�@��@��y@��\@���@�7L@�%@��`@��/@��@�I�@�b@�1@���@���@��@�33@�"�@�
=@��@��y@���@���@��u@�I�@�Z@�Q�@�I�@�Q�@��@�\)@�;d@���@���@�V@���@���@��@�`B@��@��/@�(�@��
@�;d@��R@��+@�^5@�{@�%@��9@���@�Q�@�1@�K�@���@�n�@�5?@��@���@�@�7L@��`@��j@��u@�r�@�I�@� �@�ƨ@��@�o@��H@��H@��@���@�n�@�V@�=q@�-@��@���@�`B@��@���@��u@��;@�S�@�33@�33@�C�@�S�@�"�@��@��R@��\@�n�@�$�@���@�O�@�7L@��@���@�bN@�1'@��@�  @���@��@��@�ƨ@���@��P@���@��@�ff@�{@��F@{Z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aա�Aգ�Aհ!Aմ9AլAհ!Aղ-Aղ-Aղ-AնFA���A���AվwA�ƨA���A���A���A���A���A�ƨA���Aպ^Aպ^AծAե�A՗�A�x�A�jA�dZA�S�A�G�A�5?A�JA��#A�&�A��;A���Aң�AҋDA�-A���A��yA�-A�~�A��;A�G�A͡�A�=qA�VA��`A���A�C�A�p�Aʇ+A�~�A�JA���A�dZA�+A��/Aź^A�ZA���A�M�A��DA�p�A��HA�p�A�"�A��jA���A�  A���A���A��A���A��A��RA�^5A�5?A�oA��yA�ZA�1'A�A�ffA�v�A�n�A��\A���A���A��^A�x�A�5?A��A�VA���A� �A�+A�?}A� �A��A�x�A�z�A�
=A���A��A��A��/A�%A��A�VA���A��A��-A�M�A�7LA�
=A~�DAz�+Au7LApv�Akx�AhAg/Afn�Af(�Ad��AbM�A_�wA^1'A[�mAW��AQ�AP�AO�#AM��AK�-AJ�AIl�AHJAF�AD �ABr�AA�
AAS�A@��A?�wA<�A:�A9C�A7O�A6�A3��A01'A/
=A-�TA,�A*�A'C�A%�A$��A#x�A!\)A VA�;AG�A��AM�A��A=qAK�Ax�A�AA�A��AZA�TA�A�AA�AAx�Ar�A��AG�A�HA  A�A|�A�PA
  A	&�A	VA��A	�A
ZAp�A7LA	�A	VA$�A�A�TAA�Ar�A/AK�A�AXA ȴ@�\)@���@�=q@�M�@�h@ߍP@���@�5?@�O�@�j@��m@�o@���@��m@ׅ@ו�@�\)@�v�@��@ԣ�@Ӿw@҇+@�E�@��@�V@�\)@Χ�@̴9@�@�^5@��@�?}@���@ȃ@ȣ�@�E�@�~�@��@˝�@�ƨ@˅@�t�@�\)@�n�@�/@Ǯ@��@Ƈ+@�n�@�5?@�M�@��@ũ�@ļj@��
@�o@��@�^5@���@�hs@���@���@�Z@��@��@�ƨ@�@�V@�^5@�ff@��@�hs@��@�x�@��7@�p�@�/@��D@�S�@��H@�n�@��@�hs@�7L@�Q�@�K�@�n�@�v�@���@�v�@�E�@��@�O�@���@���@��D@�9X@���@���@�o@��+@�Z@��F@�C�@���@���@��@���@���@�`B@�/@��@���@���@��@��D@�ƨ@�t�@�33@��+@���@�@�E�@�M�@�=q@��@�{@��T@���@�@��^@�@�V@���@�^5@�E�@�-@���@��-@�/@�j@�I�@�1'@��;@���@��@�C�@��@��y@��\@���@�7L@�%@��`@��/@��@�I�@�b@�1@���@���@��@�33@�"�@�
=@��@��y@���@���@��u@�I�@�Z@�Q�@�I�@�Q�@��@�\)@�;d@���@���@�V@���@���@��@�`B@��@��/@�(�@��
@�;d@��R@��+@�^5@�{@�%@��9@���@�Q�@�1@�K�@���@�n�@�5?@��@���@�@�7L@��`@��j@��u@�r�@�I�@� �@�ƨ@��@�o@��H@��H@��@���@�n�@�V@�=q@�-@��@���@�`B@��@���@��u@��;@�S�@�33@�33@�C�@�S�@�"�@��@��R@��\@�n�@�$�@���@�O�@�7L@��@���@�bN@�1'@��@�  @���@��@��@�ƨ@���@��P@���@��@�ff@�{@��F@{Z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
E�B
E�B
D�B
D�B
E�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
H�B
K�B
M�B
N�B
Q�B
S�B
XB
`BB
cTB
cTB
cTB
cTB
cTB
cTB
aHB
ZB
_;B
dZB
hsB
k�B
o�B
u�B
� B
�PB
��B
��B
�LB
��B
�B
�NB
�`B
�`B
��B�BC�BffBp�Bx�Bx�B��B�-B�BB\B�B�B"�B%�B^5B|�B��B��B��B�?B�jB�qB�XB�-B�'B�^B��B��B�wB�LB��B��B�DB�Bs�Bo�BiyB`BBe`BaHB\)BR�BF�BA�B/B��B��B��B�BdZB
��B
�B
�}B
�?B
��B
�-B
��B
�VB
{�B
ffB
T�B
C�B
(�B
%B	��B	�sB	�B	��B	��B	x�B	}�B	z�B	t�B	p�B	ffB	YB	I�B	>wB	2-B	!�B	%B	B��B��B�B�B�fB�NB�)B�B�
B�B��B��B��B��BȴBǮBĜBŢB��B�XB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B��B��B��B��B��B�'B�-B�FB�9B��B�
B�sB�B�B�yB�ZB�5B�B��B�jB�-B�?B�qB��B�qB�LBF�B��B�)B�`B�B�B�B�B�B�B�B�B�B�B��B��B	B	1B	JB	JB	DB	DB	PB	DB	\B	oB	JB	+B	+B		7B		7B	1B		7B	bB	(�B	1'B	6FB	@�B	E�B	J�B	N�B	P�B	Q�B	P�B	N�B	O�B	O�B	O�B	P�B	VB	YB	YB	XB	W
B	XB	ZB	ZB	\)B	dZB	gmB	gmB	hsB	m�B	o�B	m�B	k�B	l�B	m�B	m�B	n�B	p�B	v�B	y�B	~�B	�B	�B	�B	�7B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�3B	�9B	�qB	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�#B	�#B	�#B	�5B	�NB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
+B
1B
1B
1B
+B
+B
+B
+B
+B
1B
1B
+B
+B
+B
+B
+B
1B

=B
DB

=B
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
PB
PB
VB
VB
VB
VB
bB
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222244222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
E�B
E�B
D�B
D�B
E�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
H�B
K�B
M�B
N�B
Q�B
S�B
XB
`BB
cTB
cTB
cTB
cTB
cTB
cTB
aHB
ZB
_;B
dZB
hsB
k�B
o�B
u�B
� B
�PB
��B
��B
�LB
��B
�B
�NB
�`B
�`B
��B�BC�BffBp�Bx�Bx�B��B�-B�BB\B�B�B"�B%�B^5B|�B��B��B��B�?B�jB�qB�XB�-B�'B�^B��B��B�wB�LB��B��B�DB�Bs�Bo�BiyB`BBe`BaHB\)BR�BF�BA�B/B��B��B��B�BdZB
��B
�B
�}B
�?B
��B
�-B
��B
�VB
{�B
ffB
T�B
C�B
(�B
%B	��B	�sB	�B	��B	��B	x�B	}�B	z�B	t�B	p�B	ffB	YB	I�B	>wB	2-B	!�B	%B	B��B��B�B�B�fB�NB�)B�B�
B�B��B��B��B��BȴBǮBĜBŢB��B�XB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B��B��B��B��B��B�'B�-B�FB�9B��B�
B�sB�B�B�yB�ZB�5B�B��B�jB�-B�?B�qB��B�qB�LBF�B��B�)B�`B�B�B�B�B�B�B�B�B�B�B��B��B	B	1B	JB	JB	DB	DB	PB	DB	\B	oB	JB	+B	+B		7B		7B	1B		7B	bB	(�B	1'B	6FB	@�B	E�B	J�B	N�B	P�B	Q�B	P�B	N�B	O�B	O�B	O�B	P�B	VB	YB	YB	XB	W
B	XB	ZB	ZB	\)B	dZB	gmB	gmB	hsB	m�B	o�B	m�B	k�B	l�B	m�B	m�B	n�B	p�B	v�B	y�B	~�B	�B	�B	�B	�7B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�3B	�9B	�qB	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�#B	�#B	�#B	�5B	�NB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
+B
1B
1B
1B
+B
+B
+B
+B
+B
1B
1B
+B
+B
+B
+B
+B
1B

=B
DB

=B
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
PB
PB
VB
VB
VB
VB
bB
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222244222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190515                              AO  ARCAADJP                                                                    20181005190515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190515  QCF$                G�O�G�O�G�O�C000            