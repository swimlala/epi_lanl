CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:12Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191712  20181005191712  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               jA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���	J�1   @�����@4���E��dN�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      jA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B)33B/33B7��B@  BH  BP  BX  B_��Bh  BpffBx  B�  B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCQ�fCT  CV  CX  CZ  C\�C^�C`  Ca�fCd  Cf  Cg�fCj  Cl�Cn�Cp  Cr  Ct  Cu�fCx  Cz�C|�C~�C��C��C�  C��C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C��C��C�  C��C��3C��3C��3C�  C�  C��C��C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C��C�  C��3C�  C�  C��3C��3C�  C�  C��C��C��C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C��C�  C��3C��3C�  C��C��C�  C��D fD �fDfD�fD  Dy�D��D� DfD� D  D�fD  Dy�D��Dy�D  D� D	  D	�fD
fD
�fDfD�fD  D� D  D�fD  D� D��D� D��Dy�D��Dy�D  D�fDfD�fDfD�fDfD�fDfD� D  D�fD  D� DfD�fD  D� D  D� D  Dy�D��Dy�D��Dy�D  D� D   D �fD!  D!y�D!��D"�fD#fD#�fD$fD$� D%  D%�fD&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1�fD2fD2� D3  D3� D4  D4y�D4��D5� D6  D6y�D7  D7y�D8  D8� D8��D9y�D:  D:�fD;fD;� D<  D<y�D=  D=� D>  D>� D?fD?� D?��D@� D@��DA� DB  DB� DC  DC� DD  DD�fDE  DE� DFfDF� DG  DG� DG��DHy�DIfDI� DJ  DJ�fDK  DK� DK��DL�fDM  DMy�DN  DN� DOfDO� DO��DPy�DP��DQ� DR  DRy�DR��DS� DT  DT�fDUfDU�fDV  DVy�DW  DWy�DX  DX�fDYfDY�fDZ  DZ� D[  D[� D[��D\y�D]  D]�fD^  D^y�D^��D_� D`  D`�fDa  Da� Db  Db� DcfDc��DdfDdy�Dd��De� Df  Df� Dg  Dg�fDh  Dhy�Di  Di� DjfDj�fDk  Dk� Dl  Dly�Dl��Dmy�Dn  Dn� Do  Do� Do��Dp� DqfDq�fDr  Dr� Ds  Dsy�Ds��Dt� DufDu� Dv  Dv� Dv��Dw� DxfDy� D�0R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>�R@��\@\AG�A!G�AAG�AaG�A���A���A���A���A�p�AУ�A��A��B Q�BQ�BQ�BQ�B Q�B)�B/�B7�B@Q�BHQ�BPQ�BXQ�B_�BhQ�Bp�RBxQ�B�(�B�(�B���B�(�B�(�B�(�B�(�B���B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�C {C{C��C{C{C
{C{C{C{C{C{C��C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>.C@{CB{CD{CF{CH{CJ{CL{CN{CO��CQ��CT{CV{CX{CZ{C\.C^.C`{Ca��Cd{Cf{Cg��Cj{Cl.Cn.Cp{Cr{Ct{Cu��Cx{Cz.C|.C~.C�
C�
C�
=C�
C�
C�
=C�
=C�
=C��pC��pC��pC�
=C�
=C�
=C�
C�
=C��pC��pC��pC��pC��pC��pC�
=C�
C�
=C��pC�
=C�
C�
C�
=C�
C��pC��pC��pC�
=C�
=C�
C�
C�
=C�
=C�
C�
=C��pC��pC��pC�
=C�
=C�
=C�
C�
=C��pC�
=C�
C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC��pC�
=C�
C�
=C�
=C�
=C�
=C��pC��pC�
C�
=C��pC�
=C�
=C��pC��pC�
=C�
=C�
C�
C�
C�
=C�
=C��pC�
=C�
=C��pC�
=C�
C�
=C��pC�
C�
=C��pC��pC�
=C�
C�
C�
=C�
D �D ��D�D��DD~�D��D�D�D�DD��DD~�D��D~�DD�D	D	��D
�D
��D�D��DD�DD��DD�D��D�D��D~�D��D~�DD��D�D��D�D��D�D��D�D�DD��DD�D�D��DD�DD�DD~�D��D~�D��D~�DD�D D ��D!D!~�D!��D"��D#�D#��D$�D$�D%D%��D&�D&�D'D'�D(D(�D)D)�D*D*�D+D+��D,�D,�D-D-�D.D.�D/D/�D0�D0�D1D1��D2�D2�D3D3�D4D4~�D4��D5�D6D6~�D7D7~�D8D8�D8��D9~�D:D:��D;�D;�D<D<~�D=D=�D>D>�D?�D?�D?��D@�D@��DA�DBDB�DCDC�DDDD��DEDE�DF�DF�DGDG�DG��DH~�DI�DI�DJDJ��DKDK�DK��DL��DMDM~�DNDN�DO�DO�DO��DP~�DP��DQ�DRDR~�DR��DS�DTDT��DU�DU��DVDV~�DWDW~�DXDX��DY�DY��DZDZ�D[D[�D[��D\~�D]D]��D^D^~�D^��D_�D`D`��DaDa�DbDb�Dc�Dc��Dd�Dd~�Dd��De�DfDf�DgDg��DhDh~�DiDi�Dj�Dj��DkDk�DlDl~�Dl��Dm~�DnDn�DoDo�Do��Dp�Dq�Dq��DrDr�DsDs~�Ds��Dt�Du�Du�DvDv�Dv��Dw�Dx�Dy�D�2�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aܙ�Aܛ�Aܝ�Aܣ�Aܥ�Aܧ�Aܧ�Aܧ�Aܡ�Aܟ�Aܣ�Aܥ�Aܥ�Aܣ�Aܣ�Aܥ�Aܟ�Aܗ�A܃A�`BA�1A���A�ƨA�+A�oA���A�K�AɬA�n�A�/A���A�jA��A��A�"�A�dZA���A��yA��wA�\)A��mA�C�A�oA��A��FA���A�E�A�S�A�O�A���A���A�ZA��wA���A���A��9A��A��+A�S�A���A�~�A�-A��A���A��A�=qA��yA��7A�bA�jA��A�(�A�l�A�(�A���A�  A���A�A���A�M�A���A�l�A�|�A���A��!A�jA�hsA}\)Az�AxVAwO�Au�Atn�As�FAqO�Aml�Ak+Ai�PAgt�Af �Ae\)AdM�Ab1A`�A]��AZ�AYK�AW�AW%AVE�AUhsAT�!AT�ARVAP~�AO�hAN �AMK�AM%ALA�AK�^AJ��AI|�AHr�AGAF��AD�ADjAC�mAC|�AB�`AB�\AA��A@ffA?�^A?+A>��A=�mA;�mA9?}A81A6�DA5��A5+A4v�A1�^A-AhsA�Ar�AAƨAVA/A��A�PAG�A;dA�A$�A"�A%A
�9A	ƨA	"�A��A1'A��A�!AE�AC�AbNA�A �/A (�@�|�@��h@�j@�t�@���@��@�C�@�J@��@��@���@�Ĝ@��u@�;d@��/@�l�@�S�@�+@�E�@���@�@�@� �@�-@�A�@�\)@�+@��#@�7L@�bN@�w@��
@�n�@�^@�%@�Ĝ@��D@�j@�1@ߕ�@��@���@ܛ�@�C�@���@�t�@��@�1'@��#@�j@��
@�;d@�^5@͡�@��/@�b@��H@�V@�&�@Ǿw@�o@ƸR@�=q@�{@�@��T@ř�@��@�r�@�Q�@�9X@�1@���@öF@�K�@�@���@§�@�E�@��#@��-@�/@���@�Z@�|�@�C�@��@�J@��9@�l�@�ȴ@�@�x�@�V@�j@�  @��@�l�@��H@�M�@��@��@���@���@��h@�x�@�O�@��@���@��;@��F@���@��@���@���@�-@�
=@�?}@�;d@�hs@�|�@���@��w@�\)@��T@��@��@��^@���@�1@�33@��+@�J@�@��T@�V@���@��D@� �@�|�@���@�J@���@���@�&�@�/@�=q@�V@���@�7L@��m@�dZ@�l�@�  @���@��j@�ƨ@��w@��
@�33@���@�
=@���@�V@�ff@�=q@�{@�{@�@��^@��@�V@���@���@���@�bN@�9X@��@���@�b@� �@��F@�dZ@�dZ@�;d@��@�^5@��@�`B@�/@��@��@�V@�%@��/@��@�I�@�(�@�1@��m@�l�@�;d@���@�n�@�M�@��@���@��@���@�?}@���@��9@�z�@�r�@�Z@� �@���@�ƨ@��@��F@���@�K�@��@���@��^@��@��9@�b@�dZ@�+@�+@�K�@�9X@�  @�ff@���@�M�@���@��@��y@��y@��@��@��@��@���@��!@�ff@�=q@�$�@�@���@��-@�%@���@�(�@�C�@�v�@�-@�J@�@���@�x�@��@���@�Ĝ@���@���@��u@��u@�bN@�1'@�  @�  @��m@��F@���@�|�@�dZ@�+@��@���@�ff@�@�hs@��@���@�Z@�I�@���@�|�@�C�@�+@���@���@���@�M�@�{@�c�@k�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aܙ�Aܛ�Aܝ�Aܣ�Aܥ�Aܧ�Aܧ�Aܧ�Aܡ�Aܟ�Aܣ�Aܥ�Aܥ�Aܣ�Aܣ�Aܥ�Aܟ�Aܗ�A܃A�`BA�1A���A�ƨA�+A�oA���A�K�AɬA�n�A�/A���A�jA��A��A�"�A�dZA���A��yA��wA�\)A��mA�C�A�oA��A��FA���A�E�A�S�A�O�A���A���A�ZA��wA���A���A��9A��A��+A�S�A���A�~�A�-A��A���A��A�=qA��yA��7A�bA�jA��A�(�A�l�A�(�A���A�  A���A�A���A�M�A���A�l�A�|�A���A��!A�jA�hsA}\)Az�AxVAwO�Au�Atn�As�FAqO�Aml�Ak+Ai�PAgt�Af �Ae\)AdM�Ab1A`�A]��AZ�AYK�AW�AW%AVE�AUhsAT�!AT�ARVAP~�AO�hAN �AMK�AM%ALA�AK�^AJ��AI|�AHr�AGAF��AD�ADjAC�mAC|�AB�`AB�\AA��A@ffA?�^A?+A>��A=�mA;�mA9?}A81A6�DA5��A5+A4v�A1�^A-AhsA�Ar�AAƨAVA/A��A�PAG�A;dA�A$�A"�A%A
�9A	ƨA	"�A��A1'A��A�!AE�AC�AbNA�A �/A (�@�|�@��h@�j@�t�@���@��@�C�@�J@��@��@���@�Ĝ@��u@�;d@��/@�l�@�S�@�+@�E�@���@�@�@� �@�-@�A�@�\)@�+@��#@�7L@�bN@�w@��
@�n�@�^@�%@�Ĝ@��D@�j@�1@ߕ�@��@���@ܛ�@�C�@���@�t�@��@�1'@��#@�j@��
@�;d@�^5@͡�@��/@�b@��H@�V@�&�@Ǿw@�o@ƸR@�=q@�{@�@��T@ř�@��@�r�@�Q�@�9X@�1@���@öF@�K�@�@���@§�@�E�@��#@��-@�/@���@�Z@�|�@�C�@��@�J@��9@�l�@�ȴ@�@�x�@�V@�j@�  @��@�l�@��H@�M�@��@��@���@���@��h@�x�@�O�@��@���@��;@��F@���@��@���@���@�-@�
=@�?}@�;d@�hs@�|�@���@��w@�\)@��T@��@��@��^@���@�1@�33@��+@�J@�@��T@�V@���@��D@� �@�|�@���@�J@���@���@�&�@�/@�=q@�V@���@�7L@��m@�dZ@�l�@�  @���@��j@�ƨ@��w@��
@�33@���@�
=@���@�V@�ff@�=q@�{@�{@�@��^@��@�V@���@���@���@�bN@�9X@��@���@�b@� �@��F@�dZ@�dZ@�;d@��@�^5@��@�`B@�/@��@��@�V@�%@��/@��@�I�@�(�@�1@��m@�l�@�;d@���@�n�@�M�@��@���@��@���@�?}@���@��9@�z�@�r�@�Z@� �@���@�ƨ@��@��F@���@�K�@��@���@��^@��@��9@�b@�dZ@�+@�+@�K�@�9X@�  @�ff@���@�M�@���@��@��y@��y@��@��@��@��@���@��!@�ff@�=q@�$�@�@���@��-@�%@���@�(�@�C�@�v�@�-@�J@�@���@�x�@��@���@�Ĝ@���@���@��u@��u@�bN@�1'@�  @�  @��m@��F@���@�|�@�dZ@�+@��@���@�ff@�@�hs@��@���@�Z@�I�@���@�|�@�C�@�+@���@���@���@�M�@�{@�c�@k�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B33B2-B2-B2-B2-B33B33B33B2-B2-B2-B2-B1'B1'B1'B2-B1'B1'B0!B/B-B�B1B	7B	7BJB�B#�B;dB=qB=qBVB]/B|�B�B�%B�7B�VB��B��B��B��B�?B�wB��B�9B�B��B��B��B��B��B�{B��B�bB�By�BdZBS�BD�B49B�B�B��B��B��B�{B�PB�Bw�BhsBP�BE�B?}B1'B"�BbB
��B
�B
�`B
�/B
�B
�jB
��B
��B
�\B
{�B
J�B
8RB
0!B
)�B
!�B
�B
VB	��B	�/B	��B	�}B	�3B	��B	��B	��B	�=B	}�B	k�B	\)B	O�B	G�B	B�B	=qB	8RB	33B	/B	$�B	�B	�B	VB	
=B	1B	B	B��B��B�B�B�yB�ZB�HB�;B�/B�#B�B�B��B��B��BɺBŢB�qB�FB�'B�B�B��B��BVBiyBiyBiyBiyBm�Bp�Bo�Bm�BgmBjBhsBgmBffBgmBgmBffBffBhsBhsBiyBjBq�By�By�Bv�Bu�Bt�Bu�Bw�Bw�Bz�B{�Bz�By�Bz�B� B�B�B�B�B�B�B�B�B� B~�B~�B}�B}�B�B�B�B�%B�JB�\B�hB�{B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�?B�dB�}B��BBĜBŢBƨBƨBȴBȴB��B��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�#B�/B�/B�5B�BB�HB�NB�`B�fB�yB�B�B��B��B	B	1B	
=B	VB	hB	uB	�B	�B	�B	�B	�B	"�B	#�B	$�B	%�B	%�B	&�B	&�B	&�B	'�B	'�B	&�B	'�B	(�B	'�B	"�B	�B	�B	!�B	2-B	,B	&�B	#�B	'�B	+B	+B	+B	2-B	8RB	>wB	<jB	:^B	9XB	<jB	>wB	>wB	?}B	A�B	F�B	H�B	I�B	M�B	R�B	S�B	VB	YB	\)B	aHB	p�B	v�B	u�B	s�B	t�B	t�B	v�B	z�B	�B	�B	�B	�B	�B	�%B	�=B	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�9B	�XB	�^B	�^B	�jB	�wB	��B	B	ĜB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�BB	�HB	�HB	�HB	�5B	�)B	�B	�
B	��B	�B	�
B	�B	�;B	�HB	�5B	�5B	�HB	�TB	�`B	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�sB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B33B2-B2-B2-B2-B33B33B33B2-B2-B2-B2-B1'B1'B1'B2-B1'B1'B0!B/B-B�B1B	7B	7BJB�B#�B;dB=qB=qBVB]/B|�B�B�%B�7B�VB��B��B��B��B�?B�wB��B�9B�B��B��B��B��B��B�{B��B�bB�By�BdZBS�BD�B49B�B�B��B��B��B�{B�PB�Bw�BhsBP�BE�B?}B1'B"�BbB
��B
�B
�`B
�/B
�B
�jB
��B
��B
�\B
{�B
J�B
8RB
0!B
)�B
!�B
�B
VB	��B	�/B	��B	�}B	�3B	��B	��B	��B	�=B	}�B	k�B	\)B	O�B	G�B	B�B	=qB	8RB	33B	/B	$�B	�B	�B	VB	
=B	1B	B	B��B��B�B�B�yB�ZB�HB�;B�/B�#B�B�B��B��B��BɺBŢB�qB�FB�'B�B�B��B��BVBiyBiyBiyBiyBm�Bp�Bo�Bm�BgmBjBhsBgmBffBgmBgmBffBffBhsBhsBiyBjBq�By�By�Bv�Bu�Bt�Bu�Bw�Bw�Bz�B{�Bz�By�Bz�B� B�B�B�B�B�B�B�B�B� B~�B~�B}�B}�B�B�B�B�%B�JB�\B�hB�{B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�?B�dB�}B��BBĜBŢBƨBƨBȴBȴB��B��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�#B�/B�/B�5B�BB�HB�NB�`B�fB�yB�B�B��B��B	B	1B	
=B	VB	hB	uB	�B	�B	�B	�B	�B	"�B	#�B	$�B	%�B	%�B	&�B	&�B	&�B	'�B	'�B	&�B	'�B	(�B	'�B	"�B	�B	�B	!�B	2-B	,B	&�B	#�B	'�B	+B	+B	+B	2-B	8RB	>wB	<jB	:^B	9XB	<jB	>wB	>wB	?}B	A�B	F�B	H�B	I�B	M�B	R�B	S�B	VB	YB	\)B	aHB	p�B	v�B	u�B	s�B	t�B	t�B	v�B	z�B	�B	�B	�B	�B	�B	�%B	�=B	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�9B	�XB	�^B	�^B	�jB	�wB	��B	B	ĜB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�BB	�HB	�HB	�HB	�5B	�)B	�B	�
B	��B	�B	�
B	�B	�;B	�HB	�5B	�5B	�HB	�TB	�`B	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�sB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191712                              AO  ARCAADJP                                                                    20181005191712    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191712  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191712  QCF$                G�O�G�O�G�O�8000            