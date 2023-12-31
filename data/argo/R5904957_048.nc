CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:13Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140813  20181024140813  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               0A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׻��`�1   @׻�m�G�@3N��+�c��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      0A   A   B   @���@�  A   A   A@  A`  A���A�  A�  A�  A�  A���A�  A�  B   B  B  B��B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  D   D � D  D� D  D� D��D� D  D� D��D� DfD� D��D� D  D� D	  D	� D
fD
� D
��Dy�D��D� D  Dy�D��D� D�fDfD� D  D� D  D� D  D� D  D� D��D � D!fD!� D!��D"y�D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(fD(�fD)  D)� D)��D*� D+  D+�fD,  D,� D-  D-� D.fD.�fD/  D/� D0  D0� D1  D1� D2fD2�fD3  D3� D4  D4y�D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DM��DN� DO  DO�fDP  DP� DQ  DQ� DR  DRy�DR��DS� DS��DT� DU  DU� DV  DVy�DV��DWy�DW��DX� DX��DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`y�Da  Da� Db  Db�fDc  Dc� Dc��Ddy�De  De� Df  Df� Dg  Dg� DhfDh�fDi  Di� Dj  Dj� Dk  Dk�fDlfDl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv� Dw  Dw� Dw��Dy�HD�6fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@��A�\A"�\AB�\Ab�\A�{A�G�A�G�A�G�A�G�A�{A�G�A�G�B ��B��B��B=qB ��B(��B0��B9
=B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:\C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL\CN(�CP(�CRB�CTB�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�ChB�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C��C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C��C�{C�{C��C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�!HC�!HC�{D 
=D �=D
=D�=D
=D�=D�D�=D
=D�=D�D�=D�D�=D�D�=D
=D�=D	
=D	�=D
�D
�=D�D��D�D�=D
=D��D�D�=D��D�D�=D
=D�=D
=D�=D
=D�=D
=D�=D �D �=D!�D!�=D"�D"��D#
=D#�=D$
=D$�=D%
=D%�=D&�D&�=D'
=D'�=D(�D(��D)
=D)�=D*�D*�=D+
=D+��D,
=D,�=D-
=D-�=D.�D.��D/
=D/�=D0
=D0�=D1
=D1�=D2�D2��D3
=D3�=D4
=D4��D5�D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<�=D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB��DC
=DC�=DD
=DD�=DE
=DE�=DF
=DF��DG
=DG�=DH
=DH�=DI
=DI�=DJ
=DJ�=DK
=DK�=DL
=DL��DM
=DM�=DN�DN�=DO
=DO��DP
=DP�=DQ
=DQ�=DR
=DR��DS�DS�=DT�DT�=DU
=DU�=DV
=DV��DW�DW��DX�DX�=DY�DY��DZ
=DZ�=D[
=D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_�=D`�D`��Da
=Da�=Db
=Db��Dc
=Dc�=Dd�Dd��De
=De�=Df
=Df�=Dg
=Dg�=Dh�Dh��Di
=Di�=Dj
=Dj�=Dk
=Dk��Dl�Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do��Dp
=Dp�=Dq
=Dq�=Dr
=Dr��Ds
=Ds�=Dt
=Dt�=Du�Du��Dv
=Dv�=Dw
=Dw�=Dx�Dy��D�;�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�  A���A��A���A�n�A�=qA�9XA��yA�|�A�p�A�\)A���A�p�A�JA՛�AՅA�XA�{A��A�Q�A�ȴA�A�+A���Aɝ�A�VAǶFA�z�A�C�A�%A�S�AľwA��A��#A��;A��`A�7LA���A�+A�~�A�33A��\A�ZA�VA���A�
=A��A�n�A�7LA�9XA�v�A�A�A��A���A���A�K�A�bNA���A��A�(�A��FA�-A���A��A�K�A�hsA�|�A�A��A��PA���A�A��A���A���A�E�A��7A��A���A��A���A�^5A�"�A���A�ffA��hA��
A��A�n�A�ZA�hsA��A��A�XA�v�A��RA�&�A�FA{K�AwAs7LAox�An�+Am��Am�hAm%Ai�^Ac�hA]hsAZ�AY33AV��ATbAR�AQ��AP�jAO�PAK�AIl�AH�`AHbAG"�AF�+AE�AD��AD$�AC�#AB��AA/A>�A=�7A:�uA9"�A8{A4��A3`BA2r�A1�A/t�A. �A.�A-��A-��A-A,n�A,�A++A)�A'��A&�A%��A%�A%x�A%�A%��A%C�A$��A#t�A"�A!VA�`A~�A-A�#AA�AhsA�+AK�A-A��A;dAA�A��A;dA��A^5A�wA�mA;dA��A �AK�A~�A�
A��A�A�
Al�A
I�A	|�A�A|�AoAĜAA��Al�@���@�M�@���@�hs@���@��@�"�@��-@�X@��m@��R@���@�X@�7L@��@��@�?}@�Ĝ@�33@��@��@�9@�o@�/@�Ĝ@�A�@��;@�!@�bN@��H@�hs@�Z@�5?@��`@�Q�@۝�@��@ڗ�@�ff@�7L@�@�V@�-@�G�@�bN@�ƨ@��H@љ�@�p�@���@Гu@�A�@�1@��
@�o@�-@��@Ͳ-@�bN@�  @˥�@�
=@ɡ�@���@�I�@���@Ǯ@�\)@��y@��T@š�@Ĵ9@�(�@��m@ÍP@�S�@�o@���@�ff@���@���@�x�@�%@��@��`@��9@���@���@�K�@��@�ȴ@���@��+@�=q@�&�@�1'@��m@�K�@���@�v�@�E�@�5?@�$�@�{@���@��7@���@��@���@�S�@�o@���@�=q@��T@�@�7L@��/@���@�z�@�(�@�\)@�"�@���@�ff@�O�@�?}@�/@��@���@�Z@��@���@��P@�dZ@�C�@�
=@��@���@�V@��h@�V@��@�Z@�  @��@��@�33@���@�M�@��#@�x�@�?}@�V@���@��@�j@�I�@�  @��@�S�@�C�@�+@���@�n�@�M�@�E�@�J@��/@��D@�9X@�  @�  @���@��@��m@��
@���@�;d@��@���@�ff@�M�@�-@�J@��@��7@��@���@�bN@�b@��F@��@�\)@�K�@�33@��@���@��H@��+@�5?@�@�G�@��@��/@��@�j@�A�@���@�ƨ@��P@��@�t�@�dZ@�C�@�o@��@��H@�ȴ@�5?@��#@�hs@�&�@��/@���@�z�@� �@�b@���@��@�@��@���@���@���@���@���@���@��+@�ff@�5?@�J@���@���@�O�@�%@�Ĝ@��9@��@���@�j@�Z@�bN@�Z@�Q�@�I�@�9X@��@�1@��m@�t�@�S�@�S�@�
=@�ȴ@�v�@�-@���@��`@�A�@�|�@�C�@�@�~�@�E�@��@sv`@bff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�  A���A��A���A�n�A�=qA�9XA��yA�|�A�p�A�\)A���A�p�A�JA՛�AՅA�XA�{A��A�Q�A�ȴA�A�+A���Aɝ�A�VAǶFA�z�A�C�A�%A�S�AľwA��A��#A��;A��`A�7LA���A�+A�~�A�33A��\A�ZA�VA���A�
=A��A�n�A�7LA�9XA�v�A�A�A��A���A���A�K�A�bNA���A��A�(�A��FA�-A���A��A�K�A�hsA�|�A�A��A��PA���A�A��A���A���A�E�A��7A��A���A��A���A�^5A�"�A���A�ffA��hA��
A��A�n�A�ZA�hsA��A��A�XA�v�A��RA�&�A�FA{K�AwAs7LAox�An�+Am��Am�hAm%Ai�^Ac�hA]hsAZ�AY33AV��ATbAR�AQ��AP�jAO�PAK�AIl�AH�`AHbAG"�AF�+AE�AD��AD$�AC�#AB��AA/A>�A=�7A:�uA9"�A8{A4��A3`BA2r�A1�A/t�A. �A.�A-��A-��A-A,n�A,�A++A)�A'��A&�A%��A%�A%x�A%�A%��A%C�A$��A#t�A"�A!VA�`A~�A-A�#AA�AhsA�+AK�A-A��A;dAA�A��A;dA��A^5A�wA�mA;dA��A �AK�A~�A�
A��A�A�
Al�A
I�A	|�A�A|�AoAĜAA��Al�@���@�M�@���@�hs@���@��@�"�@��-@�X@��m@��R@���@�X@�7L@��@��@�?}@�Ĝ@�33@��@��@�9@�o@�/@�Ĝ@�A�@��;@�!@�bN@��H@�hs@�Z@�5?@��`@�Q�@۝�@��@ڗ�@�ff@�7L@�@�V@�-@�G�@�bN@�ƨ@��H@љ�@�p�@���@Гu@�A�@�1@��
@�o@�-@��@Ͳ-@�bN@�  @˥�@�
=@ɡ�@���@�I�@���@Ǯ@�\)@��y@��T@š�@Ĵ9@�(�@��m@ÍP@�S�@�o@���@�ff@���@���@�x�@�%@��@��`@��9@���@���@�K�@��@�ȴ@���@��+@�=q@�&�@�1'@��m@�K�@���@�v�@�E�@�5?@�$�@�{@���@��7@���@��@���@�S�@�o@���@�=q@��T@�@�7L@��/@���@�z�@�(�@�\)@�"�@���@�ff@�O�@�?}@�/@��@���@�Z@��@���@��P@�dZ@�C�@�
=@��@���@�V@��h@�V@��@�Z@�  @��@��@�33@���@�M�@��#@�x�@�?}@�V@���@��@�j@�I�@�  @��@�S�@�C�@�+@���@�n�@�M�@�E�@�J@��/@��D@�9X@�  @�  @���@��@��m@��
@���@�;d@��@���@�ff@�M�@�-@�J@��@��7@��@���@�bN@�b@��F@��@�\)@�K�@�33@��@���@��H@��+@�5?@�@�G�@��@��/@��@�j@�A�@���@�ƨ@��P@��@�t�@�dZ@�C�@�o@��@��H@�ȴ@�5?@��#@�hs@�&�@��/@���@�z�@� �@�b@���@��@�@��@���@���@���@���@���@���@��+@�ff@�5?@�J@���@���@�O�@�%@�Ĝ@��9@��@���@�j@�Z@�bN@�Z@�Q�@�I�@�9X@��@�1@��m@�t�@�S�@�S�@�
=@�ȴ@�v�@�-@���@��`@�A�@�|�@�C�@�@�~�@�E�@��@sv`@bff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B�{B�uB��B��B�VB�DB��B��B��B��B��B�-B�^B�XB�dB��B�`B�BBoB�B�B-B49B6FBE�BaHBo�BffBgmBgmBgmBffBn�Bm�Bk�Bl�Bo�Bn�Bp�Bu�Bv�Bu�Bq�Bp�Bo�Bl�BiyBjB�B�B�B�B�1B� Bn�BYB5?B�BuBJB��B�B�sB�BǮB��B�RB�B��B�=Bk�B]/BXBR�BM�BG�B33B�B\BB
��B
�
B
�'B
�B
��B
��B
�\B
t�B
R�B
33B
�B	��B	�/B	ŢB	�}B	�dB	�RB	�'B	��B	t�B	Q�B	C�B	;dB	0!B	#�B	�B	�B	\B	+B��B�yB�mB�ZB�HB�5B�B�
B�B�B�B��B��BȴB�}B�^B�^B�!B�B��B��B��B��B��B��B�B�B�B�B��B��B�=B��B��B��B��B�B�-B�3B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�XB��B�B�B�/B�)B��BÖB��B��BBĜB��B��B��B��B��B��B�dB�wB�jB�LB�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�'B�'B�!B�B�!B�!B�!B�!B�3B�RB�XB�XB�XB�^B�dB�jB��B��BBĜBŢBǮBȴB��B��B��B��B��B�
B�B�B�BB�TB�`B�fB�mB�sB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	@��wB	>wB	>wB	@�B	B�B	C�B	D�B	E�B	F�B	N�B	XB	ZB	^5B	aHB	cTB	e`B	ffB	ffB	ffB	hsB	jB	o�B	v�B	x�B	z�B	|�B	~�B	�B	�B	�%B	�=B	�JB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�FB	�FB	�XB	�qB	��B	B	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�B	�#B	�;B	�BB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B

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
PB
VB
PB
VB
VB
VB
VB
bB
hB
oB
{B
{B
{B
�B
�B
�B
'B
5t1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B�{B�uB��B��B�VB�DB��B��B��B��B��B�-B�^B�XB�dB��B�`B�BBoB�B�B-B49B6FBE�BaHBo�BffBgmBgmBgmBffBn�Bm�Bk�Bl�Bo�Bn�Bp�Bu�Bv�Bu�Bq�Bp�Bo�Bl�BiyBjB�B�B�B�B�1B� Bn�BYB5?B�BuBJB��B�B�sB�BǮB��B�RB�B��B�=Bk�B]/BXBR�BM�BG�B33B�B\BB
��B
�
B
�'B
�B
��B
��B
�\B
t�B
R�B
33B
�B	��B	�/B	ŢB	�}B	�dB	�RB	�'B	��B	t�B	Q�B	C�B	;dB	0!B	#�B	�B	�B	\B	+B��B�yB�mB�ZB�HB�5B�B�
B�B�B�B��B��BȴB�}B�^B�^B�!B�B��B��B��B��B��B��B�B�B�B�B��B��B�=B��B��B��B��B�B�-B�3B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�XB��B�B�B�/B�)B��BÖB��B��BBĜB��B��B��B��B��B��B�dB�wB�jB�LB�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�'B�'B�!B�B�!B�!B�!B�!B�3B�RB�XB�XB�XB�^B�dB�jB��B��BBĜBŢBǮBȴB��B��B��B��B��B�
B�B�B�BB�TB�`B�fB�mB�sB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	@��wB	>wB	>wB	@�B	B�B	C�B	D�B	E�B	F�B	N�B	XB	ZB	^5B	aHB	cTB	e`B	ffB	ffB	ffB	hsB	jB	o�B	v�B	x�B	z�B	|�B	~�B	�B	�B	�%B	�=B	�JB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�FB	�FB	�XB	�qB	��B	B	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�B	�#B	�;B	�BB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B

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
PB
VB
PB
VB
VB
VB
VB
bB
hB
oB
{B
{B
{B
�B
�B
�B
'B
5t1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140813                              AO  ARCAADJP                                                                    20181024140813    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140813  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140813  QCF$                G�O�G�O�G�O�0               