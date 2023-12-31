CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:12Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190512  20181005190512  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׵��S �1   @׵�""4v@2^5?|��c��E���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�33@���A   A   A@  A^ffA~ffA�33A�  A�  A�  A�  A�  A�33B   B  B  B  B ffB(ffB0  B8ffB@  BH  BPffBX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C��C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��C�  C�  C�  C��C��C��C��C��C��C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD�fD  D� DfD�fDfD� DfD�fD  D� D	  D	y�D
  D
� D  D� D  D� D  D�fD  D� D��D� D  D� D  D�fDfD� D  D� D��D� DfD�fDfD�fDfD� D��Dy�D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D   D �fD!  D!y�D!��D"� D#fD#� D$  D$� D%  D%� D&fD&�fD'fD'�fD(fD(�fD)  D)y�D*  D*y�D*��D+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D3��D4y�D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=fD=�fD>  D>� D?  D?� D@  D@� D@��DA� DB  DB�fDC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH�fDI  DIy�DJ  DJ� DKfDK�fDLfDL�fDMfDM� DNfDN� DN��DOy�DP  DP� DP��DQy�DR  DR� DS  DS� DS��DTy�DT��DUy�DV  DV� DWfDW�fDX  DXy�DYfDY�fDY��DZy�D[  D[y�D[��D\� D]  D]y�D^  D^� D_  D_� D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dky�DlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Du  Du�fDv  Dv� Dv��Dw� Dw� Dy��D�=D�Vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Dz�@���@�=qA�RA"�RAB�RAa�A��\A��\A�\)A�\)A�\)A�\)A�\)A��\B �B�B�B�B!zB)zB0�B9zB@�BH�BQzBX�B`�Bh�Bp�Bx�B�#�B�W
B�W
B�W
B�W
B�W
B�W
B��=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B��=B��=C +�C+�C+�C+�C+�C
+�C+�C+�C+�CECEC+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch�Cj+�Cl+�Cn+�Cp�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~EC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C�"�C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C��C��C��C��C��D 
�D ��D
�D��D
�D��DGD�GD
�D��DGD�GDGD��DGD�GD
�D��D	
�D	�{D

�D
��D
�D��D
�D��D
�D�GD
�D��D{D��D
�D��D
�D�GDGD��D
�D��D{D��DGD�GDGD�GDGD��D{D�{D
�D��D
�D��D
�D��D
�D�{D
�D��D
�D��D
�D�{D 
�D �GD!
�D!�{D"{D"��D#GD#��D$
�D$��D%
�D%��D&GD&�GD'GD'�GD(GD(�GD)
�D)�{D*
�D*�{D+{D+��D,GD,�GD-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3�{D4{D4�{D5
�D5��D6{D6��D7
�D7��D8
�D8��D9
�D9��D:GD:��D;
�D;��D<
�D<��D=GD=�GD>
�D>��D?
�D?��D@
�D@��DA{DA��DB
�DB�GDC
�DC��DD
�DD��DE
�DE�GDF
�DF��DG
�DG��DH
�DH�GDI
�DI�{DJ
�DJ��DKGDK�GDLGDL�GDMGDM��DNGDN��DO{DO�{DP
�DP��DQ{DQ�{DR
�DR��DS
�DS��DT{DT�{DU{DU�{DV
�DV��DWGDW�GDX
�DX�{DYGDY�GDZ{DZ�{D[
�D[�{D\{D\��D]
�D]�{D^
�D^��D_
�D_��D`
�D`��DaGDa��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg{Dg��Dh
�Dh��Di
�Di��Dj
�Dj�GDk
�Dk�{DlGDl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt�{Du
�Du�GDv
�Dv��Dw{Dw��Dw��Dy��D�B�D�[�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԏ\Aԕ�AԍPAԋDAԏ\Aԣ�Aԕ�Aԟ�Aԣ�Aԥ�Aԧ�Aԥ�Aԩ�AԮAԮA԰!AԬAԬAԬAԮAԲ-AԴ9AԴ9AԲ-AԴ9AԴ9AԬAԃA�?}A��TA�M�A�ȴA�XA�AѬA�=qA��HAЃA�(�A�ĜA�ĜA΃A�?}A���A��ÁA���A�VA�ȴA�dZA�=qA��A��HAɍPAȟ�A�-A�bNA��yA�M�A�33A�bNA�jA�1A¥�A�bA�E�A��jA�r�A�?}A�v�A���A�C�A�?}A��
A��+A���A��wA��A��\A�$�A�ĜA��A�A���A�ZA��A�&�A�M�A�S�A��A�$�A��jA��FA�ƨA�E�A��!A�O�A��wA��-A��A� �A��!A�Q�A��`A�JA��A���A|��Aw�#Aq�Al�Ag�Ab�A^�jA\�9A[x�AZ^5AX��AW�AV9XAU�AT��AT=qASƨARJANZAK33AI��AHI�AF~�AF5?AE�TAEx�AC�FAA
=A>ĜA$�RA#�wA#p�A"n�A �jA  AS�A�\A(�A  A��A�TA�hA7LA%A��A�A1'Al�A��A(�A��AȴA�PA+AZA7LA(�A��A�Az�A�mA5?AXA
��A
ffA	�A�A�\A�hAM�Ax�A�`A~�A��A
=A(�AA r�@�t�@��7@�V@�j@�S�@�J@�o@��@���@���@�@��^@�;d@��@�&�@�(�@�ff@�^@�Z@�\)@�v�@��#@�hs@��@���@�I�@ߥ�@�o@�M�@�?}@��@�o@�{@ܬ@�h@��@�+@��H@�D@��@��`@�hs@��@�1@�|�@�33@���@֧�@�ȴ@���@���@�Ĝ@��/@��@�J@��@�"�@ٙ�@��/@���@���@��
@���@��@���@߅@��@��@݁@܃@�(�@۝�@�
=@��@��@؃@��m@��H@���@��;@͡�@���@�b@��@��
@���@ǥ�@���@�V@ċD@�(�@�ƨ@�O�@�Ĝ@�j@�S�@���@��y@���@��@���@�G�@��`@��/@��u@��@��@��R@�5?@��@�{@���@��@��#@���@��^@���@��h@�X@���@�Q�@��@�\)@�+@�33@�o@�ff@��^@��@���@��u@�(�@�@��T@�x�@��@���@��@�  @��m@�ƨ@��P@�C�@�n�@�?}@�Z@�b@��@��;@��w@���@�\)@�;d@�"�@���@���@��\@���@�@��h@��@��@�hs@�X@�7L@���@�z�@�Q�@�ƨ@�\)@�C�@�\)@�dZ@�\)@�C�@�+@�o@��H@���@���@�M�@��#@�x�@�`B@�O�@��@�z�@�Z@�I�@�  @��@�\)@��@���@���@��+@�v�@�ȴ@��@�E�@�V@�O�@���@�z�@��m@�+@��\@��7@�X@�V@���@��u@�A�@���@�S�@�o@���@��R@��!@���@��\@��+@�$�@���@��@��`@���@���@��u@�bN@���@�S�@��y@��y@��@�o@��@�"�@��R@�M�@��@���@�7L@�z�@�Q�@�1@���@���@��@�|�@�t�@�l�@�dZ@�\)@�K�@�"�@�@��R@��\@�ff@�-@�{@��T@�x�@�`B@�`B@�`B@�O�@�7L@�7L@�&�@��@��/@�A�@��F@�|�@��@��y@�ff@�@�$�@��@��^@�X@�V@�Ĝ@�bN@�ƨ@�W�@�W@lS�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aԏ\Aԕ�AԍPAԋDAԏ\Aԣ�Aԕ�Aԟ�Aԣ�Aԥ�Aԧ�Aԥ�Aԩ�AԮAԮA԰!AԬAԬAԬAԮAԲ-AԴ9AԴ9AԲ-AԴ9AԴ9AԬAԃA�?}A��TA�M�A�ȴA�XA�AѬA�=qA��HAЃA�(�A�ĜA�ĜA΃A�?}A���A��ÁA���A�VA�ȴA�dZA�=qA��A��HAɍPAȟ�A�-A�bNA��yA�M�A�33A�bNA�jA�1A¥�A�bA�E�A��jA�r�A�?}A�v�A���A�C�A�?}A��
A��+A���A��wA��A��\A�$�A�ĜA��A�A���A�ZA��A�&�A�M�A�S�A��A�$�A��jA��FA�ƨA�E�A��!A�O�A��wA��-A��A� �A��!A�Q�A��`A�JA��A���A|��Aw�#Aq�Al�Ag�Ab�A^�jA\�9A[x�AZ^5AX��AW�AV9XAU�AT��AT=qASƨARJANZAK33AI��AHI�AF~�AF5?AE�TAEx�AC�FAA
=A>ĜA$�RA#�wA#p�A"n�A �jA  AS�A�\A(�A  A��A�TA�hA7LA%A��A�A1'Al�A��A(�A��AȴA�PA+AZA7LA(�A��A�Az�A�mA5?AXA
��A
ffA	�A�A�\A�hAM�Ax�A�`A~�A��A
=A(�AA r�@�t�@��7@�V@�j@�S�@�J@�o@��@���@���@�@��^@�;d@��@�&�@�(�@�ff@�^@�Z@�\)@�v�@��#@�hs@��@���@�I�@ߥ�@�o@�M�@�?}@��@�o@�{@ܬ@�h@��@�+@��H@�D@��@��`@�hs@��@�1@�|�@�33@���@֧�@�ȴ@���@���@�Ĝ@��/@��@�J@��@�"�@ٙ�@��/@���@���@��
@���@��@���@߅@��@��@݁@܃@�(�@۝�@�
=@��@��@؃@��m@��H@���@��;@͡�@���@�b@��@��
@���@ǥ�@���@�V@ċD@�(�@�ƨ@�O�@�Ĝ@�j@�S�@���@��y@���@��@���@�G�@��`@��/@��u@��@��@��R@�5?@��@�{@���@��@��#@���@��^@���@��h@�X@���@�Q�@��@�\)@�+@�33@�o@�ff@��^@��@���@��u@�(�@�@��T@�x�@��@���@��@�  @��m@�ƨ@��P@�C�@�n�@�?}@�Z@�b@��@��;@��w@���@�\)@�;d@�"�@���@���@��\@���@�@��h@��@��@�hs@�X@�7L@���@�z�@�Q�@�ƨ@�\)@�C�@�\)@�dZ@�\)@�C�@�+@�o@��H@���@���@�M�@��#@�x�@�`B@�O�@��@�z�@�Z@�I�@�  @��@�\)@��@���@���@��+@�v�@�ȴ@��@�E�@�V@�O�@���@�z�@��m@�+@��\@��7@�X@�V@���@��u@�A�@���@�S�@�o@���@��R@��!@���@��\@��+@�$�@���@��@��`@���@���@��u@�bN@���@�S�@��y@��y@��@�o@��@�"�@��R@�M�@��@���@�7L@�z�@�Q�@�1@���@���@��@�|�@�t�@�l�@�dZ@�\)@�K�@�"�@�@��R@��\@�ff@�-@�{@��T@�x�@�`B@�`B@�`B@�O�@�7L@�7L@�&�@��@��/@�A�@��F@�|�@��@��y@�ff@�@�$�@��@��^@�X@�V@�Ĝ@�bN@�ƨ@�W�@�W@lS�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
K�B
L�B
N�B
O�B
R�B
XB
bNB
n�B
� B
�B
�=B
|�B
T�B
aHB
�\B
��B
��B
��B
�'B
�qB
�HB
�B
��BB1BDB�B#�B7LB=qBF�B^5Bq�B�%B�JB�oB��B�B��B%B	7BoB!�B6FBB�B[#BaHBm�B}�B� BVBB��B�VB�B��B�%BcTBF�B7LB�B
��B
�HB
��B
��B
�B
~�B
w�B
r�B
jB
_;B
YB
o�B
m�B
cTB
XB
A�B
�B	�B	��B	�B	y�B	S�B	+B	{B	B	B	JB	%B	  B��B�B�TB�#B��B��B��BŢB��BÖB��BȴBĜB��B�qB�^BÖB�jB�B�B�B��B�uB�7B�VB�hB�hB�hB�oB�{B�{B�{B�{B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�?B�RB�}BƨB��B��B��BɺBɺB��BɺBǮBƨBǮB��B��B�B	JB	oB	�B	{B	�B	%�B	uB	B��B�B�B�B��B��B��B	  B	  B	B	+B	JB	�B	�B	!�B	33B	N�B	N�B	P�B	ffB	o�B	r�B	p�B	o�B	p�B	s�B	s�B	u�B	w�B	z�B	{�B	z�B	y�B	t�B	p�B	k�B	bNB	XB	T�B	T�B	[#B	[#B	ZB	ZB	R�B	N�B	J�B	G�B	E�B	B�B	=qB	>wB	@�B	A�B	C�B	B�B	D�B	E�B	F�B	I�B	I�B	J�B	K�B	M�B	M�B	Q�B	S�B	S�B	T�B	T�B	T�B	VB	W
B	W
B	W
B	W
B	XB	[#B	_;B	dZB	gmB	p�B	t�B	w�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�VB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�?B	�LB	�LB	�LB	�RB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�jB	�qB	�wB	��B	B	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	�B	�B	�;B	�;B	�5B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�sB	�mB	�fB	�fB	�fB	�sB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
	7B
�B
�B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
K�B
L�B
N�B
O�B
R�B
XB
bNB
n�B
� B
�B
�=B
|�B
T�B
aHB
�\B
��B
��B
��B
�'B
�qB
�HB
�B
��BB1BDB�B#�B7LB=qBF�B^5Bq�B�%B�JB�oB��B�B��B%B	7BoB!�B6FBB�B[#BaHBm�B}�B� BVBB��B�VB�B��B�%BcTBF�B7LB�B
��B
�HB
��B
��B
�B
~�B
w�B
r�B
jB
_;B
YB
o�B
m�B
cTB
XB
A�B
�B	�B	��B	�B	y�B	S�B	+B	{B	B	B	JB	%B	  B��B�B�TB�#B��B��B��BŢB��BÖB��BȴBĜB��B�qB�^BÖB�jB�B�B�B��B�uB�7B�VB�hB�hB�hB�oB�{B�{B�{B�{B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�?B�RB�}BƨB��B��B��BɺBɺB��BɺBǮBƨBǮB��B��B�B	JB	oB	�B	{B	�B	%�B	uB	B��B�B�B�B��B��B��B	  B	  B	B	+B	JB	�B	�B	!�B	33B	N�B	N�B	P�B	ffB	o�B	r�B	p�B	o�B	p�B	s�B	s�B	u�B	w�B	z�B	{�B	z�B	y�B	t�B	p�B	k�B	bNB	XB	T�B	T�B	[#B	[#B	ZB	ZB	R�B	N�B	J�B	G�B	E�B	B�B	=qB	>wB	@�B	A�B	C�B	B�B	D�B	E�B	F�B	I�B	I�B	J�B	K�B	M�B	M�B	Q�B	S�B	S�B	T�B	T�B	T�B	VB	W
B	W
B	W
B	W
B	XB	[#B	_;B	dZB	gmB	p�B	t�B	w�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�VB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�?B	�LB	�LB	�LB	�RB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�jB	�qB	�wB	��B	B	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	�B	�B	�;B	�;B	�5B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�sB	�mB	�fB	�fB	�fB	�sB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
	7B
�B
�B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190512                              AO  ARCAADJP                                                                    20181005190512    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190512  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190512  QCF$                G�O�G�O�G�O�8000            