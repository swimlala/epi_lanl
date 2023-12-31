CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:37Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140837  20181024140837  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$��}�1   @��%hK��@5�V��c��+J1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @333@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BHffBPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cc�fCe�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D%��D&� D'  D'� D(  D(� D)  D)�fD*fD*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<y�D<��D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DU��DV� DW  DW� DW��DXy�DY  DY� DZ  DZ� DZ��D[� D\  D\� D\��D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db�fDcfDc�fDdfDd� De  Dey�Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dw�3Dy��D�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=p�@��@��A ��A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B
=B��B��B��B ��B(��B0��B8��B@��BI
=BQ
=BY
=B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�CB�CB�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,\C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB\CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`B�Cb(�Cd\Cf\Ch\Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv\Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C��C�{C�{C�!HC�!HC�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�!HC�!HC�{C�{C�{D 
=D �=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D�D�=D
=D�=D	
=D	�=D

=D
�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D��D�D��D
=D�=D
=D�=D
=D��D
=D�=D
=D�=D
=D��D
=D�=D
=D�=D
=D��D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D 
=D �=D!
=D!�=D"
=D"�=D#
=D#�=D$
=D$��D%
=D%�=D&�D&�=D'
=D'�=D(
=D(�=D)
=D)��D*�D*�=D+
=D+��D,
=D,�=D-
=D-�=D.
=D.�=D/
=D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3
=D3�=D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9
=D9�=D:�D:�=D;
=D;�=D<
=D<��D=�D=�=D>
=D>�=D?
=D?�=D@
=D@��DA
=DA�=DB
=DB�=DC
=DC�=DD
=DD�=DE
=DE�=DF
=DF�=DG
=DG�=DH�DH�=DI
=DI�=DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM��DN
=DN�=DO
=DO�=DP
=DP�=DQ
=DQ�=DR
=DR�=DS
=DS�=DT
=DT�=DU�DU�=DV�DV�=DW
=DW�=DX�DX��DY
=DY�=DZ
=DZ�=D[�D[�=D\
=D\�=D]�D]�=D^
=D^�=D_
=D_��D`
=D`�=Da
=Da�=Db
=Db��Dc�Dc��Dd�Dd�=De
=De��Df
=Df��Dg
=Dg�=Dh
=Dh�=Di
=Di�=Dj
=Dj��Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do�=Dp
=Dp�=Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dt�=Du
=Du�=Dv�Dv�=Dw
=Dw�=Dw�pDy��D�B>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�\)A�^5A�\)A�\)A�`BA�`BA�bNA�bNA�dZA�ffA�ffA�hsA�hsA�jA�hsA�hsA�hsA�jA�hsA�ffA�ffA�dZA�^5A�K�A�C�A�?}A�5?A�oA��/Aϧ�A�`BA�(�A���A� �A��A���A�~�A���A�A�S�A��A��!A��A��A��HA���A�t�A��A�ƨA�/A�K�A�jA��7A�7LA�ffA�|�A��A���A��7A�l�A��^A���A�$�A��A���A�+A���A��+A�jA�\)A�?}A��HA�oA�v�A�{A��A�A�v�A�S�A�G�A��A��FA�jA���A��RA�33A{�Ay33Aw��Aw��Aw�hAw|�AvE�Ar�Ap��Am��Ak`BAi�PAc/A`1'A]�#A[��AY��AX�yAW|�AU�AR�APVAOXAN��AN��AN��AN�ALv�AKt�AJ�AH�AH �AG��AF�uAE�#AEC�AD��AD9XAB��ABQ�AAt�A?�A;�A8�RA7hsA6��A6��A6��A6ȴA6��A6��A6�A5+A4jA3`BA0{A.jA-+A+�
A+�A*�uA)��A'�A%&�A#��A"�+A!��A ��At�A��A?}A�AhsA�A�A"�A�wAG�A�A�hAdZA�Av�A9XA�TA�FAG�AI�AA��A��A
��A	K�A�RAM�A1A��Al�AG�A�/A~�A�A�A��AA �D@��\@��H@��@��F@�&�@��@�A�@��D@�F@�dZ@�+@��@@���@��/@�b@���@�ƨ@�\)@���@�ȴ@�\@�E�@陚@��;@�S�@��@��H@�@��@�ȴ@�V@��@�/@�Z@�l�@�\@�M�@��@���@�o@�7L@�K�@�M�@�o@�n�@�=q@��@��#@�@���@��T@ѡ�@с@�@���@ѩ�@�V@��;@�Q�@ʧ�@��@���@��@ə�@�x�@ɉ7@ə�@���@���@�C�@ƸR@�n�@�V@��@Ł@��@ċD@ċD@Ĵ9@��`@�&�@�G�@�`B@��@��`@Ĭ@Ý�@��@���@��H@��@�v�@���@��j@�bN@� �@�ƨ@�K�@�+@�
=@��@�ȴ@���@�=q@�?}@���@�r�@�A�@�  @��F@�^5@�hs@�A�@��w@�33@���@�M�@���@�X@�?}@�&�@�V@�V@��@��@��@��@��@��@�%@��D@��
@�"�@�$�@��7@�G�@�/@��@��@��@��R@��@��#@���@�7L@�Ĝ@���@�dZ@�"�@���@�~�@�hs@���@��@��P@�
=@��+@���@�hs@�O�@��`@�1'@��@�33@���@�~�@�$�@��@���@�O�@��@��/@���@��j@��u@�j@�1'@���@��m@��m@��
@��F@���@�S�@��@�=q@�{@��@���@�O�@���@��9@���@��@���@���@���@���@�Q�@��@�ƨ@��P@�S�@��R@�n�@�$�@���@�/@��@�V@���@�j@��@�ƨ@�ƨ@���@�|�@�l�@�K�@��@��!@�5?@���@��#@�X@���@�1'@��@��
@��F@���@�dZ@�;d@�
=@�~�@��@�{@�@���@���@�Z@���@�"�@��R@�V@�@��^@���@�`B@��@���@��D@�A�@��w@��@�"�@���@���@��;@��F@��@���@��@�dZ@�33@���@��\@��+@�5?@���@��@��#@���@���@��-@��7@�Ĝ@��@��@�U2@o�r1111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�\)A�^5A�\)A�\)A�`BA�`BA�bNA�bNA�dZA�ffA�ffA�hsA�hsA�jA�hsA�hsA�hsA�jA�hsA�ffA�ffA�dZA�^5A�K�A�C�A�?}A�5?A�oA��/Aϧ�A�`BA�(�A���A� �A��A���A�~�A���A�A�S�A��A��!A��A��A��HA���A�t�A��A�ƨA�/A�K�A�jA��7A�7LA�ffA�|�A��A���A��7A�l�A��^A���A�$�A��A���A�+A���A��+A�jA�\)A�?}A��HA�oA�v�A�{A��A�A�v�A�S�A�G�A��A��FA�jA���A��RA�33A{�Ay33Aw��Aw��Aw�hAw|�AvE�Ar�Ap��Am��Ak`BAi�PAc/A`1'A]�#A[��AY��AX�yAW|�AU�AR�APVAOXAN��AN��AN��AN�ALv�AKt�AJ�AH�AH �AG��AF�uAE�#AEC�AD��AD9XAB��ABQ�AAt�A?�A;�A8�RA7hsA6��A6��A6��A6ȴA6��A6��A6�A5+A4jA3`BA0{A.jA-+A+�
A+�A*�uA)��A'�A%&�A#��A"�+A!��A ��At�A��A?}A�AhsA�A�A"�A�wAG�A�A�hAdZA�Av�A9XA�TA�FAG�AI�AA��A��A
��A	K�A�RAM�A1A��Al�AG�A�/A~�A�A�A��AA �D@��\@��H@��@��F@�&�@��@�A�@��D@�F@�dZ@�+@��@@���@��/@�b@���@�ƨ@�\)@���@�ȴ@�\@�E�@陚@��;@�S�@��@��H@�@��@�ȴ@�V@��@�/@�Z@�l�@�\@�M�@��@���@�o@�7L@�K�@�M�@�o@�n�@�=q@��@��#@�@���@��T@ѡ�@с@�@���@ѩ�@�V@��;@�Q�@ʧ�@��@���@��@ə�@�x�@ɉ7@ə�@���@���@�C�@ƸR@�n�@�V@��@Ł@��@ċD@ċD@Ĵ9@��`@�&�@�G�@�`B@��@��`@Ĭ@Ý�@��@���@��H@��@�v�@���@��j@�bN@� �@�ƨ@�K�@�+@�
=@��@�ȴ@���@�=q@�?}@���@�r�@�A�@�  @��F@�^5@�hs@�A�@��w@�33@���@�M�@���@�X@�?}@�&�@�V@�V@��@��@��@��@��@��@�%@��D@��
@�"�@�$�@��7@�G�@�/@��@��@��@��R@��@��#@���@�7L@�Ĝ@���@�dZ@�"�@���@�~�@�hs@���@��@��P@�
=@��+@���@�hs@�O�@��`@�1'@��@�33@���@�~�@�$�@��@���@�O�@��@��/@���@��j@��u@�j@�1'@���@��m@��m@��
@��F@���@�S�@��@�=q@�{@��@���@�O�@���@��9@���@��@���@���@���@���@�Q�@��@�ƨ@��P@�S�@��R@�n�@�$�@���@�/@��@�V@���@�j@��@�ƨ@�ƨ@���@�|�@�l�@�K�@��@��!@�5?@���@��#@�X@���@�1'@��@��
@��F@���@�dZ@�;d@�
=@�~�@��@�{@�@���@���@�Z@���@�"�@��R@�V@�@��^@���@�`B@��@���@��D@�A�@��w@��@�"�@���@���@��;@��F@��@���@��@�dZ@�33@���@��\@��+@�5?@���@��@��#@���@���@��-@��7@�Ĝ@��@��@�U2@o�r1111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�)B�)B�)B�)B�)B�)B�)B�/B�BB�HB�NB�TB�sB�B  BhB��B��B�B�'B�XB�dB�XB�FB�jB�qB�jB�3B�'B�!B�B�B��B�{B�DB�Bw�BiyB<jB�B��B�B�B�B�mB�/B��B�FB��B�%Bv�Bq�Bo�Bm�Bl�Bk�BdZBH�BVB
�B
��B
ǮB
��B
�!B
�{B
u�B
e`B
S�B
L�B
G�B
=qB
�B
JB
B
  B	��B	��B	�B	�#B	��B	�LB	��B	�bB	jB	R�B	B�B	8RB	/B	(�B	�B	uB	PB	JB	
=B	DB	DB	
=B		7B	+B	B	B��B��B��B��B��B�B�B�B�yB�fB�NB�B��BǮBÖBB��B��B��B��B�}B�qB�^B�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�7B�B�B�B�B�B~�B�B�B�Bz�Br�Bt�Bm�Bl�Bk�BjBiyBffBdZBaHB^5B[#BXBW
BVBT�BT�BS�BS�BR�BP�BO�BN�BM�BP�BS�BS�BQ�BW
BXBZB`BBhsBw�B|�B|�B}�B}�B}�B|�Bz�B�B�VB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�RB�qB��B��BBȴB��B��B��B��B��B��B��B��B�B�B�B�/B�BB�HB�`B�fB�fB�fB�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	B	%B	DB	\B	bB	hB	oB	uB	�B	!�B	(�B	,B	0!B	2-B	49B	7LB	:^B	;dB	;dB	<jB	<jB	;dB	;dB	;dB	<jB	;dB	;dB	;dB	>wB	B�B	G�B	O�B	VB	YB	YB	ZB	YB	dZB	l�B	r�B	u�B	u�B	u�B	v�B	v�B	v�B	v�B	u�B	t�B	v�B	v�B	w�B	v�B	v�B	v�B	v�B	w�B	v�B	v�B	w�B	y�B	y�B	z�B	{�B	|�B	}�B	~�B	�%B	�1B	�=B	�DB	�DB	�JB	�VB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�9B	�FB	�RB	�XB	�dB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�5B	�;B	�HB	�NB	�fB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
�1111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�)B�)B�)B�)B�)B�)B�)B�/B�BB�HB�NB�TB�sB�B  BhB��B��B�B�'B�XB�dB�XB�FB�jB�qB�jB�3B�'B�!B�B�B��B�{B�DB�Bw�BiyB<jB�B��B�B�B�B�mB�/B��B�FB��B�%Bv�Bq�Bo�Bm�Bl�Bk�BdZBH�BVB
�B
��B
ǮB
��B
�!B
�{B
u�B
e`B
S�B
L�B
G�B
=qB
�B
JB
B
  B	��B	��B	�B	�#B	��B	�LB	��B	�bB	jB	R�B	B�B	8RB	/B	(�B	�B	uB	PB	JB	
=B	DB	DB	
=B		7B	+B	B	B��B��B��B��B��B�B�B�B�yB�fB�NB�B��BǮBÖBB��B��B��B��B�}B�qB�^B�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�7B�B�B�B�B�B~�B�B�B�Bz�Br�Bt�Bm�Bl�Bk�BjBiyBffBdZBaHB^5B[#BXBW
BVBT�BT�BS�BS�BR�BP�BO�BN�BM�BP�BS�BS�BQ�BW
BXBZB`BBhsBw�B|�B|�B}�B}�B}�B|�Bz�B�B�VB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�RB�qB��B��BBȴB��B��B��B��B��B��B��B��B�B�B�B�/B�BB�HB�`B�fB�fB�fB�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	B	%B	DB	\B	bB	hB	oB	uB	�B	!�B	(�B	,B	0!B	2-B	49B	7LB	:^B	;dB	;dB	<jB	<jB	;dB	;dB	;dB	<jB	;dB	;dB	;dB	>wB	B�B	G�B	O�B	VB	YB	YB	ZB	YB	dZB	l�B	r�B	u�B	u�B	u�B	v�B	v�B	v�B	v�B	u�B	t�B	v�B	v�B	w�B	v�B	v�B	v�B	v�B	w�B	v�B	v�B	w�B	y�B	y�B	z�B	{�B	|�B	}�B	~�B	�%B	�1B	�=B	�DB	�DB	�JB	�VB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�9B	�FB	�RB	�XB	�dB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�5B	�;B	�HB	�NB	�fB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
�1111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140837                              AO  ARCAADJP                                                                    20181024140837    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140837  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140837  QCF$                G�O�G�O�G�O�4000            