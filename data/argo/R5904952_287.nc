CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:11Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190611  20181005190611  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�)���1   @�*O�@@1��hr�!�c��j~��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�ff@�33A   AffA>ffA^ffA~ffA�33A�33A�33A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~�C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D � D  D� D  D� D  Dy�D  D� D  D� DfD�fDfD�fD  D� D��D	� D
  D
y�D  D� D  D� D  D�fD  D� D  D� D  Dy�D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��Dy�D  D� D   D � D!  D!y�D"  D"� D#  D#� D$fD$�fD%fD%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8fD8� D9  D9� D:fD:� D:��D;� D<fD<� D<��D=� D>  D>� D?  D?�fD@fD@�fDA  DAy�DB  DB� DC  DC� DD  DD�fDEfDE�fDFfDF�fDG  DG� DH  DH�fDI  DIy�DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DYy�DZ  DZ� D[  D[y�D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� D`��Day�Da��Dby�Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dq� Dr  Dry�Dr��Dsy�Dt  Dt� DufDu�fDv  Dv� Dw  Dw� Dx  DxFfDy�)D�S�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�z�A ��A
=A?
=A_
=A
=A��A��A��A�Q�A�Q�A�Q�A�B (�B(�B(�B(�B (�B((�B0(�B8�\B@�\BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C�C
=C
=C
=C
=C
=C
=C#�C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz#�C|
=C~#�C�C�C��C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC��RC�C��C�C�C�C�C��C��C�C��RC�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C��C�C�C�C�C��RC�C�C�C�C��RC��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C��C��C�C�C��C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC��RD ��D�D��D�D��D�D|)D�D��D�D��D�D��D�D��D�D��D�)D	��D
�D
|)D�D��D�D��D�D��D�D��D�D��D�D|)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D|)D�D��D�D��D�)D|)D�D��D �D ��D!�D!|)D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+|)D,�D,��D-�D-|)D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D:�)D;��D<�D<��D<�)D=��D>�D>��D?�D?��D@�D@��DA�DA|)DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI|)DI�)DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY|)DZ�DZ��D[�D[|)D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��D`�)Da|)Da�)Db|)Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq��Dr�Dr|)Dr�)Ds|)Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�DxH�Dy��D�T�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�{A��A�"�A�+A�-A�+A�+A�-A�-A�-A�/A�/A�/A�/A�1'A�1'A�33A�33A�1'A�33A�$�A�`BA��A��HA��
A���Aɩ�A�ffA��A���A�{A�S�A�`BA��yA�=qA��TAǑhA�"�AƁA�z�A�ffA�M�A��/Aŧ�AŋDA�(�A��
A�Q�Að!A��A�hsA���A�r�A�"�A���A�x�A��A��/A���A�r�A��-A���A��A�A�A�S�A�z�A�n�A��mA���A���A��A���A��A�bNA�ƨA�hsA�ffA��TA�VA�5?A��7A��A��wA��\A�A�A��A� �A�%A��-A��^A��!A���A��A���A�1'A�XA�M�A��+A��A��A���A�bNA��PA� �A��A�O�A��
A��A� �A��FA�t�A�AyAw��As|�Ao�#AmAl(�Ag�Ae�Aa33A^A�A\��A[
=AV^5AS�AQt�AO|�AK�7AGƨAFȴAFM�AD��AB�A@ZA=�^A;�A;&�A:r�A6�HA5�A5K�A3�A0A.��A-�
A,ȴA*�\A)�;A(�`A&�+A$A"�RA"A�A ^5A�RAK�A�9A��A�\AZA9XA��A�AdZA�jAZAVA1A��AC�A�A�`A�mA�9Ap�A�\A�
AhsAG�A��AE�A�A��A�\A1'A�A{A��A�-A��A
�DA
VA	�A	\)A{A��A�!A �A��A33A%A%AA�A{A\)A�Az�A�-A �@��R@�X@�G�@���@�@��-@�X@��m@���@�G�@�/@�&�@��@�  @��@�hs@��@��@�bN@�+@��@�\)@�R@��@��@�O�@㕁@��m@��
@��m@�I�@�9@�33@��@�@�E�@���@�+@�ff@�ȴ@��H@���@�
=@�
=@�V@�&�@�9@�&�@���@�G�@�Q�@߾w@�C�@�"�@�o@���@�v�@�V@�-@��T@�G�@���@�+@��#@�7L@�V@��/@���@�r�@ו�@�S�@�+@�@��H@֧�@�v�@�^5@�^5@��#@Ցh@�G�@���@�r�@���@ӥ�@�o@���@�E�@���@�A�@��
@��y@��@���@�Ĝ@���@̓u@�1'@� �@���@��;@˶F@˕�@�33@���@ʗ�@�M�@��@�X@���@��/@ȣ�@�bN@���@ǥ�@�l�@�t�@���@Ɨ�@�n�@���@�X@�%@���@ă@�A�@��@�  @�  @���@Å@�C�@���@§�@�@�~�@�E�@���@�?}@�Ĝ@��@��u@��D@�Z@���@�S�@�S�@�
=@�^5@���@���@�X@�%@��9@��@�Z@�I�@�b@��;@��w@���@�l�@�+@�ȴ@�=q@�J@���@���@�`B@��@���@�z�@�1'@�t�@��H@���@��!@��!@���@�@�bN@�1'@� �@��@���@��w@��y@�ff@�M�@�$�@���@��7@�O�@��@��`@�z�@�Z@�I�@�9X@���@�ƨ@�t�@�o@��+@�{@���@�7L@�Ĝ@�I�@��F@��@�S�@��H@�~�@�V@�$�@���@���@��^@��@��/@�9X@�|�@�"�@�
=@��!@�V@�=q@��@��@��^@��@�G�@��9@��m@��@��H@�n�@�-@���@��@���@�r�@��@���@�n�@�5?@�{@��#@��h@�7L@��/@��@�(�@���@�K�@�C�@�C�@�;d@��@�
=@��y@�^5@�`B@���@��u@�9X@�1@��
@���@�dZ@��@���@��y@�ȴ@�v�@�5?@���@�?}@�%@��D@��@��@��@��@w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�{A��A�"�A�+A�-A�+A�+A�-A�-A�-A�/A�/A�/A�/A�1'A�1'A�33A�33A�1'A�33A�$�A�`BA��A��HA��
A���Aɩ�A�ffA��A���A�{A�S�A�`BA��yA�=qA��TAǑhA�"�AƁA�z�A�ffA�M�A��/Aŧ�AŋDA�(�A��
A�Q�Að!A��A�hsA���A�r�A�"�A���A�x�A��A��/A���A�r�A��-A���A��A�A�A�S�A�z�A�n�A��mA���A���A��A���A��A�bNA�ƨA�hsA�ffA��TA�VA�5?A��7A��A��wA��\A�A�A��A� �A�%A��-A��^A��!A���A��A���A�1'A�XA�M�A��+A��A��A���A�bNA��PA� �A��A�O�A��
A��A� �A��FA�t�A�AyAw��As|�Ao�#AmAl(�Ag�Ae�Aa33A^A�A\��A[
=AV^5AS�AQt�AO|�AK�7AGƨAFȴAFM�AD��AB�A@ZA=�^A;�A;&�A:r�A6�HA5�A5K�A3�A0A.��A-�
A,ȴA*�\A)�;A(�`A&�+A$A"�RA"A�A ^5A�RAK�A�9A��A�\AZA9XA��A�AdZA�jAZAVA1A��AC�A�A�`A�mA�9Ap�A�\A�
AhsAG�A��AE�A�A��A�\A1'A�A{A��A�-A��A
�DA
VA	�A	\)A{A��A�!A �A��A33A%A%AA�A{A\)A�Az�A�-A �@��R@�X@�G�@���@�@��-@�X@��m@���@�G�@�/@�&�@��@�  @��@�hs@��@��@�bN@�+@��@�\)@�R@��@��@�O�@㕁@��m@��
@��m@�I�@�9@�33@��@�@�E�@���@�+@�ff@�ȴ@��H@���@�
=@�
=@�V@�&�@�9@�&�@���@�G�@�Q�@߾w@�C�@�"�@�o@���@�v�@�V@�-@��T@�G�@���@�+@��#@�7L@�V@��/@���@�r�@ו�@�S�@�+@�@��H@֧�@�v�@�^5@�^5@��#@Ցh@�G�@���@�r�@���@ӥ�@�o@���@�E�@���@�A�@��
@��y@��@���@�Ĝ@���@̓u@�1'@� �@���@��;@˶F@˕�@�33@���@ʗ�@�M�@��@�X@���@��/@ȣ�@�bN@���@ǥ�@�l�@�t�@���@Ɨ�@�n�@���@�X@�%@���@ă@�A�@��@�  @�  @���@Å@�C�@���@§�@�@�~�@�E�@���@�?}@�Ĝ@��@��u@��D@�Z@���@�S�@�S�@�
=@�^5@���@���@�X@�%@��9@��@�Z@�I�@�b@��;@��w@���@�l�@�+@�ȴ@�=q@�J@���@���@�`B@��@���@�z�@�1'@�t�@��H@���@��!@��!@���@�@�bN@�1'@� �@��@���@��w@��y@�ff@�M�@�$�@���@��7@�O�@��@��`@�z�@�Z@�I�@�9X@���@�ƨ@�t�@�o@��+@�{@���@�7L@�Ĝ@�I�@��F@��@�S�@��H@�~�@�V@�$�@���@���@��^@��@��/@�9X@�|�@�"�@�
=@��!@�V@�=q@��@��@��^@��@�G�@��9@��m@��@��H@�n�@�-@���@��@���@�r�@��@���@�n�@�5?@�{@��#@��h@�7L@��/@��@�(�@���@�K�@�C�@�C�@�;d@��@�
=@��y@�^5@�`B@���@��u@�9X@�1@��
@���@�dZ@��@���@��y@�ȴ@�v�@�5?@���@�?}@�%@��D@��@��@��@��@w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	'�B	,B	l�B	��B	��B	�B
\B
(�B
A�B
L�B
[#B
hsB
�B
��B
��B
��B
�B
�'B
ÖB
�;B
�B
�B
�BB�B,B>wBF�BP�BM�BF�BF�BM�BhsBl�Bv�B�1B�bB��B��B��B�!B�jBB�)B��B��B�B�B�B��B0!B9XBA�BG�BP�B5?B:^B,B49B@�B@�BB�B6FB33B&�B+B�B�)B��BB�LB��B��B�Bo�BXB2-B�B%B
�B
�B
�B
��B
ÖB
��B
�{B
�B
l�B
D�B
&�B
bB	��B	�`B	�3B	��B	y�B	aHB	R�B	H�B	49B	$�B	bB	B��B�B�HB�
B��BĜB�dB�9B�'B�B�B��B��B�B�LB�XB�LB�wB��B�}B�dBB��B��BɺBȴB��B��B��BǮBĜB��B�}B�jBB��B��B��B�
B�HB�B�B��B	+B	PB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	oB	PB	DB	
=B		7B	PB	oB	{B	�B	�B	&�B	#�B	#�B	$�B	,B	,B	2-B	-B	/B	2-B	1'B	5?B	?}B	A�B	D�B	K�B	K�B	L�B	K�B	G�B	A�B	=qB	@�B	A�B	<jB	;dB	A�B	J�B	K�B	N�B	O�B	Q�B	Q�B	P�B	M�B	L�B	R�B	S�B	S�B	R�B	R�B	M�B	H�B	F�B	E�B	@�B	<jB	?}B	D�B	I�B	K�B	N�B	P�B	N�B	K�B	M�B	O�B	T�B	e`B	m�B	r�B	t�B	u�B	u�B	v�B	u�B	s�B	u�B	z�B	�B	�1B	�1B	�JB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�}B	��B	��B	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�5B	�;B	�;B	�;B	�;B	�BB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
\B

�B
qB
&�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	'�B	,B	l�B	��B	��B	�B
\B
(�B
A�B
L�B
[#B
hsB
�B
��B
��B
��B
�B
�'B
ÖB
�;B
�B
�B
�BB�B,B>wBF�BP�BM�BF�BF�BM�BhsBl�Bv�B�1B�bB��B��B��B�!B�jBB�)B��B��B�B�B�B��B0!B9XBA�BG�BP�B5?B:^B,B49B@�B@�BB�B6FB33B&�B+B�B�)B��BB�LB��B��B�Bo�BXB2-B�B%B
�B
�B
�B
��B
ÖB
��B
�{B
�B
l�B
D�B
&�B
bB	��B	�`B	�3B	��B	y�B	aHB	R�B	H�B	49B	$�B	bB	B��B�B�HB�
B��BĜB�dB�9B�'B�B�B��B��B�B�LB�XB�LB�wB��B�}B�dBB��B��BɺBȴB��B��B��BǮBĜB��B�}B�jBB��B��B��B�
B�HB�B�B��B	+B	PB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	oB	PB	DB	
=B		7B	PB	oB	{B	�B	�B	&�B	#�B	#�B	$�B	,B	,B	2-B	-B	/B	2-B	1'B	5?B	?}B	A�B	D�B	K�B	K�B	L�B	K�B	G�B	A�B	=qB	@�B	A�B	<jB	;dB	A�B	J�B	K�B	N�B	O�B	Q�B	Q�B	P�B	M�B	L�B	R�B	S�B	S�B	R�B	R�B	M�B	H�B	F�B	E�B	@�B	<jB	?}B	D�B	I�B	K�B	N�B	P�B	N�B	K�B	M�B	O�B	T�B	e`B	m�B	r�B	t�B	u�B	u�B	v�B	u�B	s�B	u�B	z�B	�B	�1B	�1B	�JB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�}B	��B	��B	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�5B	�;B	�;B	�;B	�;B	�BB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
\B

�B
qB
&�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190611                              AO  ARCAADJP                                                                    20181005190611    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190611  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190611  QCF$                G�O�G�O�G�O�8000            