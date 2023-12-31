CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:17Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140817  20181024140817  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               GA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��e	=�1   @��e��QV@2"M����c����l�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      GA   A   A   @�33@�  A   A   A@  A^ffA�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BW��B`  Bh  Bp  BxffB�  B���B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD�fD  D�fD  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
fD
�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fD  D� D fD �fD!  D!� D"  D"� D#  D#� D$  D$� D$��D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DKy�DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DRy�DS  DS� DTfDT�fDU  DU� DV  DVy�DV��DWy�DX  DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`fD`�fDa  Da� Db  Dby�Db��Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dgy�Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�ED�x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�p�A�RA"�RAB�RAa�A�\)A�(�A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BPG�BXG�B`�Bh�Bp�ByzB�W
B�#�B�#�B�W
B�W
B�W
B�W
B�W
B��=B��=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
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
B�#�C +�CEC+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�CEC+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZEC\+�C^+�C`+�Cb+�CdECf+�Ch+�Cj+�Cl+�Cn+�CpECr+�Ct+�Cv+�CxECz+�C|+�C~+�C��C��C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��DGD�GD
�D�GD
�D��D
�D��D
�D��D
�D�{D
�D��D
�D��D	
�D	��D
GD
�GD
�D��DGD��D
�D��D
�D��D
�D��D
�D��D
�D��DGD��D
�D��D
�D��D
�D��DGD�GD
�D��D
�D��D
�D��D
�D��D{D��D
�D��D
�D��D
�D�GD
�D��D GD �GD!
�D!��D"
�D"��D#
�D#��D$
�D$��D%{D%�{D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*�{D+
�D+��D,
�D,��D-
�D-��D.{D.��D/
�D/��D0
�D0��D1
�D1��D2{D2��D3
�D3�{D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>�{D?
�D?�GD@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ{DJ��DK
�DK�{DL
�DL�GDMGDM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR{DR�{DS
�DS��DTGDT�GDU
�DU��DV
�DV�{DW{DW�{DX
�DX�{DY{DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_{D_��D`GD`�GDa
�Da��Db
�Db�{Dc{Dc��Dd
�Dd�GDe
�De��Df
�Df��Dg
�Dg�{Dh{Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Du
�Du��Dv
�Dv��Dw
�Dw��Dw�GDy�fD�J�D�~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A�+A�+A�%A���A��;Aݗ�AݍPA݃A�v�A�n�A�`BA�VA�VA�O�A�M�A�K�A�I�A�E�A�?}A�(�A��`A܍PA�ȴA٥�A��TA�l�A�`BA�|�A�1'A��AмjA�|�A��HAˡ�A˅A�A�n�A�O�A�ȴAȏ\A�\)A�^5A�+A�-A��A�^5A�;dA�dZA�ĜA��A�S�A�bA��yA��!A�?}A� �A���A��jA��A���A��hA��wA�1A���A�hsA�JA���A��7A�jA���A�x�A���A��hA�(�A��A�\)A���A���A�A��^A��A�;dA��jA�v�A�5?A��hA�oA��+A���A�^5A�v�A���A�VA�/A�  A��A���A�A��yA� �A��FA���A���A��-A�;dA���A�JA�ffA��\A�  A�=qA�ffA��A���At�A|I�AxĜAs��AqS�Ao7LAmO�AkoAh�9Ad�A`�+A]XAX��AUAP$�AN��AM��AMALI�AK&�AG\)AD��ADE�ADbAB��A@Q�A?dZA;?}A:�+A:A�A9�A8��A5�
A4ĜA29XA/�A.ȴA-�mA-�A-?}A,bNA)��A&bNA$bNA#��A#G�A"��A!�TA!hsA ��A ��A �uA�A�#A�
AĜA�TAdZA�\AI�A9XA�;A�FA�RAJA�FA�hA�A�mAz�AbNA��A��A��A��A�/An�A?}A�A
�RA|�A�yA�AZAl�A��A�#A$�A�A �A ��AO�A{A"�A&�A�A�@�\)@�J@��D@�S�@���@��@�Q�@��@�V@�bN@�E�@�-@��T@� �@�t�@���@��@��`@�@�@�K�@�\@�@��/@�\)@��#@�@�@��m@�+@���@݁@��@��@�v�@��@׮@�$�@ՙ�@� �@�S�@�"�@��y@�v�@���@�z�@�1@�  @�\)@���@��@�z�@���@ɡ�@�Ĝ@�1'@ƸR@��@�J@�@Ł@�X@��/@�A�@î@��@�V@���@�hs@�&�@��u@�I�@�ƨ@���@�dZ@��@�`B@��@��P@��+@�hs@���@���@�1@�C�@���@�=q@�ff@�$�@���@���@�`B@�p�@��^@���@�7L@���@���@���@�p�@�%@�bN@�b@��+@���@���@���@�
=@�n�@��@��@��@�1@�|�@�
=@��!@��+@��@���@�p�@��@��9@��@�(�@��@��m@�dZ@�
=@�v�@�/@��9@���@�bN@�9X@�  @���@��P@��P@��P@�;d@���@��+@�~�@�{@��@���@�V@�Ĝ@��@��@�bN@�I�@�1'@�b@��m@��w@��@���@��@�dZ@�t�@�l�@�dZ@�"�@���@���@��!@�E�@�$�@�G�@��@���@���@�K�@�;d@�;d@�o@�ff@��h@��/@��`@��/@��@� �@��w@�dZ@���@�M�@���@���@���@�X@�V@���@�A�@��w@�t�@�S�@�33@�o@���@�J@���@�@�@���@�/@�V@���@��9@�j@�Z@�I�@�  @�K�@���@�v�@�M�@�J@��T@���@��-@���@�O�@�V@�Ĝ@��@�(�@��w@��@���@��P@��P@��@�|�@�"�@�ȴ@�ff@�5?@��@��-@��@�G�@���@�1@�ƨ@�t�@�\)@�K�@�33@�@���@��-@�x�@��@�b@��F@�C�@���@���@�V@�@���@��7@�G�@�Ĝ@�bN@�Z@�j@�j@+@}��@}�@}O�@|�j@|�@{��@{t�@z�\@zn�@z^5@z=q@y��@x�O@i�C@\<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�+A�+A�%A���A��;Aݗ�AݍPA݃A�v�A�n�A�`BA�VA�VA�O�A�M�A�K�A�I�A�E�A�?}A�(�A��`A܍PA�ȴA٥�A��TA�l�A�`BA�|�A�1'A��AмjA�|�A��HAˡ�A˅A�A�n�A�O�A�ȴAȏ\A�\)A�^5A�+A�-A��A�^5A�;dA�dZA�ĜA��A�S�A�bA��yA��!A�?}A� �A���A��jA��A���A��hA��wA�1A���A�hsA�JA���A��7A�jA���A�x�A���A��hA�(�A��A�\)A���A���A�A��^A��A�;dA��jA�v�A�5?A��hA�oA��+A���A�^5A�v�A���A�VA�/A�  A��A���A�A��yA� �A��FA���A���A��-A�;dA���A�JA�ffA��\A�  A�=qA�ffA��A���At�A|I�AxĜAs��AqS�Ao7LAmO�AkoAh�9Ad�A`�+A]XAX��AUAP$�AN��AM��AMALI�AK&�AG\)AD��ADE�ADbAB��A@Q�A?dZA;?}A:�+A:A�A9�A8��A5�
A4ĜA29XA/�A.ȴA-�mA-�A-?}A,bNA)��A&bNA$bNA#��A#G�A"��A!�TA!hsA ��A ��A �uA�A�#A�
AĜA�TAdZA�\AI�A9XA�;A�FA�RAJA�FA�hA�A�mAz�AbNA��A��A��A��A�/An�A?}A�A
�RA|�A�yA�AZAl�A��A�#A$�A�A �A ��AO�A{A"�A&�A�A�@�\)@�J@��D@�S�@���@��@�Q�@��@�V@�bN@�E�@�-@��T@� �@�t�@���@��@��`@�@�@�K�@�\@�@��/@�\)@��#@�@�@��m@�+@���@݁@��@��@�v�@��@׮@�$�@ՙ�@� �@�S�@�"�@��y@�v�@���@�z�@�1@�  @�\)@���@��@�z�@���@ɡ�@�Ĝ@�1'@ƸR@��@�J@�@Ł@�X@��/@�A�@î@��@�V@���@�hs@�&�@��u@�I�@�ƨ@���@�dZ@��@�`B@��@��P@��+@�hs@���@���@�1@�C�@���@�=q@�ff@�$�@���@���@�`B@�p�@��^@���@�7L@���@���@���@�p�@�%@�bN@�b@��+@���@���@���@�
=@�n�@��@��@��@�1@�|�@�
=@��!@��+@��@���@�p�@��@��9@��@�(�@��@��m@�dZ@�
=@�v�@�/@��9@���@�bN@�9X@�  @���@��P@��P@��P@�;d@���@��+@�~�@�{@��@���@�V@�Ĝ@��@��@�bN@�I�@�1'@�b@��m@��w@��@���@��@�dZ@�t�@�l�@�dZ@�"�@���@���@��!@�E�@�$�@�G�@��@���@���@�K�@�;d@�;d@�o@�ff@��h@��/@��`@��/@��@� �@��w@�dZ@���@�M�@���@���@���@�X@�V@���@�A�@��w@�t�@�S�@�33@�o@���@�J@���@�@�@���@�/@�V@���@��9@�j@�Z@�I�@�  @�K�@���@�v�@�M�@�J@��T@���@��-@���@�O�@�V@�Ĝ@��@�(�@��w@��@���@��P@��P@��@�|�@�"�@�ȴ@�ff@�5?@��@��-@��@�G�@���@�1@�ƨ@�t�@�\)@�K�@�33@�@���@��-@�x�@��@�b@��F@�C�@���@���@�V@�@���@��7@�G�@�Ĝ@�bN@�Z@�j@�j@+@}��@}�@}O�@|�j@|�@{��@{t�@z�\@zn�@z^5@z=q@y��@x�O@i�C@\<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
�qB
ĜB
�BPBK�Bl�B�B��B��B��B��B��B�FB�wBÖB�HB	7B\B$�BL�BcTBx�B�=B��B�FBÖBȴB��B��B��B��B��BǮBÖB��B�dB�LB�FB�?B�9B�!B��BɺB��B��BȴBŢBŢBŢBĜB��B�jB�LB�'B��B��B�oB�VB|�Bq�Bm�BiyBe`B^5BO�BE�B:^B$�BoB��B�BB��BɺBĜB�}B�dB�!B|�B`BBXBD�B0!B)�B �B�B%B
�yB
��B
��B
�LB
��B
�DB
q�B
T�B
A�B
)�B
PB	�B	�TB	��B	ƨB	�FB	��B	�+B	hsB	ZB	D�B	2-B	�B	�B	uB	\B	DB	B��B�B�sB�fB�BB�B��BɺBƨBŢBB�wB�XB�FB�9B�B�B�B�B��B��B��B�DB�B|�B}�B�B�B�B�B� B�B�B�oB��B��B��B�uB�bB�oB�uB�{B��B��B��B��B��B��B��B�B�wBƨB��BƨBŢB�#B�BB��B�XB��B~�Bx�Bx�By�Bv�Bq�Bl�BgmBgmBk�Bn�B�hB��B��B��B��B��B�{B�bB�VB��B��B��B��B�B�?B�RB�9B�9B�3B�-B�!B�!B�B��B��B��B�B�B�B�B�B�B�B�9B�FB�LB�XB�^B�^B�dB�wB��BÖBƨBǮB��B��B��B��B��B��B�B�B�B�B�5B�;B�BB�`B�yB�B�B��B��B��B��B��B��B��B	B	B	1B	PB	bB	oB	uB	�B	�B	�B	�B	 �B	%�B	'�B	)�B	'�B	%�B	&�B	+B	-B	.B	.B	1'B	2-B	6FB	7LB	6FB	8RB	9XB	A�B	E�B	I�B	J�B	N�B	Q�B	R�B	S�B	VB	XB	ZB	W
B	W
B	YB	^5B	`BB	dZB	n�B	q�B	r�B	q�B	s�B	x�B	|�B	|�B	|�B	~�B	�B	�B	�%B	�1B	�=B	�=B	�JB	�hB	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�FB	�RB	�dB	�dB	�^B	�dB	��B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�/B	�;B	�HB	�BB	�HB	�;B	�)B	�NB	�`B	�`B	�sB	�sB	�yB	�B	�sB	�sB	�B	�B	�B	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
B
B
%B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
PB
VB
hB
hB
hB
{B
�B
�B
�B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
1�B
;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
�qB
ĜB
�BPBK�Bl�B�B��B��B��B��B��B�FB�wBÖB�HB	7B\B$�BL�BcTBx�B�=B��B�FBÖBȴB��B��B��B��B��BǮBÖB��B�dB�LB�FB�?B�9B�!B��BɺB��B��BȴBŢBŢBŢBĜB��B�jB�LB�'B��B��B�oB�VB|�Bq�Bm�BiyBe`B^5BO�BE�B:^B$�BoB��B�BB��BɺBĜB�}B�dB�!B|�B`BBXBD�B0!B)�B �B�B%B
�yB
��B
��B
�LB
��B
�DB
q�B
T�B
A�B
)�B
PB	�B	�TB	��B	ƨB	�FB	��B	�+B	hsB	ZB	D�B	2-B	�B	�B	uB	\B	DB	B��B�B�sB�fB�BB�B��BɺBƨBŢBB�wB�XB�FB�9B�B�B�B�B��B��B��B�DB�B|�B}�B�B�B�B�B� B�B�B�oB��B��B��B�uB�bB�oB�uB�{B��B��B��B��B��B��B��B�B�wBƨB��BƨBŢB�#B�BB��B�XB��B~�Bx�Bx�By�Bv�Bq�Bl�BgmBgmBk�Bn�B�hB��B��B��B��B��B�{B�bB�VB��B��B��B��B�B�?B�RB�9B�9B�3B�-B�!B�!B�B��B��B��B�B�B�B�B�B�B�B�9B�FB�LB�XB�^B�^B�dB�wB��BÖBƨBǮB��B��B��B��B��B��B�B�B�B�B�5B�;B�BB�`B�yB�B�B��B��B��B��B��B��B��B	B	B	1B	PB	bB	oB	uB	�B	�B	�B	�B	 �B	%�B	'�B	)�B	'�B	%�B	&�B	+B	-B	.B	.B	1'B	2-B	6FB	7LB	6FB	8RB	9XB	A�B	E�B	I�B	J�B	N�B	Q�B	R�B	S�B	VB	XB	ZB	W
B	W
B	YB	^5B	`BB	dZB	n�B	q�B	r�B	q�B	s�B	x�B	|�B	|�B	|�B	~�B	�B	�B	�%B	�1B	�=B	�=B	�JB	�hB	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�FB	�RB	�dB	�dB	�^B	�dB	��B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�/B	�;B	�HB	�BB	�HB	�;B	�)B	�NB	�`B	�`B	�sB	�sB	�yB	�B	�sB	�sB	�B	�B	�B	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
B
B
%B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
PB
VB
hB
hB
hB
{B
�B
�B
�B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
1�B
;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140817                              AO  ARCAADJP                                                                    20181024140817    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140817  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140817  QCF$                G�O�G�O�G�O�0               