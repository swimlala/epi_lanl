CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:27Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141427  20181024141427  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��$�~]=1   @��%<���@3Rn��O��c�O�;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy��D�H�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@C�
@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�G�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<(�C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cu��Cx\Cz\C{��C~\C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D

=D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
=D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
=D��D�D��D�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$
=D$��D%�D%��D&�D&��D'�D'�=D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw}qDy��D�J�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A�A�A�A�A�A���A���A�JA�
=A�
=A�
=A�1A�1A�1A�JA�
=A�1A�1A�1A�A�A���A���A���A��A��mA��;A���A���Aװ!Aס�Aח�A׏\Aׇ+A�v�A�l�A�jA�ffA�\)A�C�A֕�A�\)A��A�~�A�K�A�9XA��A��A��A��A�$�A�AΧ�A�{A��HAǲ-A�I�A�=qA�v�A���A�;dA��A���A���A�VA�n�A�{A�1'A��-A�-A��-A���A�S�A��`A�v�A�-A�z�A�VA��mA�ZA�A�hsA�+A��hA���A�A�A��A�A��HA�;dA��hA�z�A�-A���A�bA�7LA�Q�A��mA��9A���A�XA�ZA�`BA��A��-A�7LA��A��9A�t�A���A�bA�^A}AyS�Av�Aq��AooAm�^Al�RAlM�Ak`BAi��Agt�Ab�AY;dAR�AP��AO��AM�AK�AIK�AFZAB�+A?+A<�A:z�A7��A6ȴA4=qA2$�A0��A/\)A-&�A,1A+"�A)�A)�PA)
=A(�\A(n�A'��A't�A'"�A&��A&�HA&��A%t�A#�A#x�A"�/A!oA��A�A�+AVA�mAO�A��An�A�A��AA�+AA�RA��A��AA\)A�A��AƨA`BAC�Av�A{A�#AXA
�+A
�A	"�AA�AAĜA�A�TA��A�+A�A33A ~�@��@��F@�\)@�M�@��@�K�@���@�~�@��-@�Ĝ@� �@�o@�J@�Z@�33@�-@���@�!@���@�j@�1@�t�@�n�@�9@睲@��y@�X@���@ާ�@�-@�?}@�`B@݉7@�V@�o@�@��@�33@֧�@ՙ�@�V@ԃ@�l�@ҧ�@�x�@ЋD@�(�@Ѓ@��;@�o@�`B@̼j@�bN@�  @��;@˅@��H@��@�O�@Ȭ@��;@�\)@��@�~�@�ff@�V@�hs@Ĭ@��m@�@��@�V@��D@�b@��P@�33@���@��@��^@�`B@��@���@���@��@�I�@��@���@��P@�S�@��@��+@�5?@�{@���@��D@�b@��P@�33@���@��@��^@�`B@��@���@���@��@�I�@��@���@��P@�S�@��@��+@�5?@�{@���@��#@�x�@�&�@��@�%@��`@��@��@�Q�@��m@�ƨ@���@�"�@�
=@��@��@���@��R@���@��R@��+@�v�@�n�@�=q@��^@�/@���@�1'@�+@��!@�E�@�-@�J@���@��-@��7@��7@��@�`B@�O�@���@��@�Z@�+@���@�=q@�J@�@�X@���@�b@��@���@�K�@�"�@�"�@�o@�@��@���@�$�@�p�@��9@�j@�bN@�bN@�r�@��@�9X@�1@���@��w@�dZ@���@��\@�M�@�{@���@��@��@��@���@��j@� �@�t�@�=q@�{@��#@��h@��/@��@��@��@�1'@��@�\)@�"�@���@��H@�~�@�V@��@�%@���@�A�@��@��P@���@�J@���@��`@�z�@�(�@��m@���@�ƨ@�ƨ@��w@��F@���@�dZ@��y@���@�ff@�5?@��#@��^@���@��@���@��D@�Q�@��@�1@�1@�ƨ@���@���@��@�|�@�K�@��y@��\@�@�=q@�J@���@��-@�hs@�O�@�O�@�?}@���@�Q�@��@�A�@�9X@���@�"�@��@��@�ȴ@���@��!@�n�@�-@�@�p�@�O�@���@��D@�j@�j@�(�@��F@��P@�t�@�t�@�t�@�l�@�l�@�dZ@�dZ@�\)@�S�@�;d@�+@��y@��H@���@���@���@�~�@�n�@��F@��@v��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A�A�A�A�A�A���A���A�JA�
=A�
=A�
=A�1A�1A�1A�JA�
=A�1A�1A�1A�A�A���A���A���A��A��mA��;A���A���Aװ!Aס�Aח�A׏\Aׇ+A�v�A�l�A�jA�ffA�\)A�C�A֕�A�\)A��A�~�A�K�A�9XA��A��A��A��A�$�A�AΧ�A�{A��HAǲ-A�I�A�=qA�v�A���A�;dA��A���A���A�VA�n�A�{A�1'A��-A�-A��-A���A�S�A��`A�v�A�-A�z�A�VA��mA�ZA�A�hsA�+A��hA���A�A�A��A�A��HA�;dA��hA�z�A�-A���A�bA�7LA�Q�A��mA��9A���A�XA�ZA�`BA��A��-A�7LA��A��9A�t�A���A�bA�^A}AyS�Av�Aq��AooAm�^Al�RAlM�Ak`BAi��Agt�Ab�AY;dAR�AP��AO��AM�AK�AIK�AFZAB�+A?+A<�A:z�A7��A6ȴA4=qA2$�A0��A/\)A-&�A,1A+"�A)�A)�PA)
=A(�\A(n�A'��A't�A'"�A&��A&�HA&��A%t�A#�A#x�A"�/A!oA��A�A�+AVA�mAO�A��An�A�A��AA�+AA�RA��A��AA\)A�A��AƨA`BAC�Av�A{A�#AXA
�+A
�A	"�AA�AAĜA�A�TA��A�+A�A33A ~�@��@��F@�\)@�M�@��@�K�@���@�~�@��-@�Ĝ@� �@�o@�J@�Z@�33@�-@���@�!@���@�j@�1@�t�@�n�@�9@睲@��y@�X@���@ާ�@�-@�?}@�`B@݉7@�V@�o@�@��@�33@֧�@ՙ�@�V@ԃ@�l�@ҧ�@�x�@ЋD@�(�@Ѓ@��;@�o@�`B@̼j@�bN@�  @��;@˅@��H@��@�O�@Ȭ@��;@�\)@��@�~�@�ff@�V@�hs@Ĭ@��m@�@��@�V@��D@�b@��P@�33@���@��@��^@�`B@��@���@���@��@�I�@��@���@��P@�S�@��@��+@�5?@�{@���@��D@�b@��P@�33@���@��@��^@�`B@��@���@���@��@�I�@��@���@��P@�S�@��@��+@�5?@�{@���@��#@�x�@�&�@��@�%@��`@��@��@�Q�@��m@�ƨ@���@�"�@�
=@��@��@���@��R@���@��R@��+@�v�@�n�@�=q@��^@�/@���@�1'@�+@��!@�E�@�-@�J@���@��-@��7@��7@��@�`B@�O�@���@��@�Z@�+@���@�=q@�J@�@�X@���@�b@��@���@�K�@�"�@�"�@�o@�@��@���@�$�@�p�@��9@�j@�bN@�bN@�r�@��@�9X@�1@���@��w@�dZ@���@��\@�M�@�{@���@��@��@��@���@��j@� �@�t�@�=q@�{@��#@��h@��/@��@��@��@�1'@��@�\)@�"�@���@��H@�~�@�V@��@�%@���@�A�@��@��P@���@�J@���@��`@�z�@�(�@��m@���@�ƨ@�ƨ@��w@��F@���@�dZ@��y@���@�ff@�5?@��#@��^@���@��@���@��D@�Q�@��@�1@�1@�ƨ@���@���@��@�|�@�K�@��y@��\@�@�=q@�J@���@��-@�hs@�O�@�O�@�?}@���@�Q�@��@�A�@�9X@���@�"�@��@��@�ȴ@���@��!@�n�@�-@�@�p�@�O�@���@��D@�j@�j@�(�@��F@��P@�t�@�t�@�t�@�l�@�l�@�dZ@�dZ@�\)@�S�@�;d@�+@��y@��H@���@���@���@�~�@�n�@��F@��@v��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BH�BG�BG�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BJ�BJ�BK�BL�BM�BP�BR�BVBYB[#B^5BaHBe`BhsBiyBjBl�Bn�B{�B��B��B��B�B�B�B�B�?B��B�HB��B��B�B-B]/By�B�1B�{B��B��B��B��B��B��B��B��B�oB�By�BjBr�Bx�Bu�BiyBe`BbNBdZBiyBjBgmBgmB[#BC�B2-BVB�B�#B�qB�B��B��B�Bk�BZB6FBhB
��B
ƨB
��B
�3B
��B
��B
�PB
�B
v�B
n�B
YB
@�B
)�B
�B
oB
5?B
33B
�B

=B	�B	�BB	�
B	��B	��B	ǮB	�dB	��B	�1B	D�B	PB��B��B��B��B�B�`B��BɺBB�jB�FB�!B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�?B�LB�?B�LB�3B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�\B�{B��B��B��B��B��B�{B�hB�bB�hB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B�hB��B��B��B�B�B�B�B�9B�RB�qB��BBÖBŢBȴBȴBƨBĜBɺB�B�)B�)B�)B�)B�5B�)B�)B�#B��B��B��B�B�B�#B�/B�B��B��BȴBƨBǮBƨBǮB��B��B��B��B��B�/B�)B�/B�BB�TB�fB�fB�fB�sB�yB�B�B��B��B	B	B	%B	+B	+B	PB	hB	�B	�B	 �B	#�B	'�B	+B	-B	.B	0!B	49B	6FB	8RB	<jB	>wB	?}B	@�B	A�B	C�B	F�B	G�B	H�B	K�B	M�B	N�B	N�B	N�B	'�B	+B	-B	.B	0!B	49B	6FB	8RB	<jB	>wB	?}B	@�B	A�B	C�B	F�B	G�B	H�B	K�B	M�B	N�B	N�B	N�B	N�B	P�B	R�B	S�B	T�B	W
B	XB	[#B	]/B	]/B	]/B	^5B	aHB	bNB	cTB	cTB	dZB	hsB	iyB	k�B	m�B	n�B	n�B	o�B	q�B	q�B	s�B	t�B	t�B	u�B	y�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�1B	�=B	�=B	�bB	�hB	�hB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�?B	�RB	�^B	�jB	B	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�HB	�HB	�HB	�TB	�TB	�ZB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
�B
�B
!�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BH�BG�BG�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BJ�BJ�BK�BL�BM�BP�BR�BVBYB[#B^5BaHBe`BhsBiyBjBl�Bn�B{�B��B��B��B�B�B�B�B�?B��B�HB��B��B�B-B]/By�B�1B�{B��B��B��B��B��B��B��B��B�oB�By�BjBr�Bx�Bu�BiyBe`BbNBdZBiyBjBgmBgmB[#BC�B2-BVB�B�#B�qB�B��B��B�Bk�BZB6FBhB
��B
ƨB
��B
�3B
��B
��B
�PB
�B
v�B
n�B
YB
@�B
)�B
�B
oB
5?B
33B
�B

=B	�B	�BB	�
B	��B	��B	ǮB	�dB	��B	�1B	D�B	PB��B��B��B��B�B�`B��BɺBB�jB�FB�!B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�?B�LB�?B�LB�3B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�\B�{B��B��B��B��B��B�{B�hB�bB�hB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B�hB��B��B��B�B�B�B�B�9B�RB�qB��BBÖBŢBȴBȴBƨBĜBɺB�B�)B�)B�)B�)B�5B�)B�)B�#B��B��B��B�B�B�#B�/B�B��B��BȴBƨBǮBƨBǮB��B��B��B��B��B�/B�)B�/B�BB�TB�fB�fB�fB�sB�yB�B�B��B��B	B	B	%B	+B	+B	PB	hB	�B	�B	 �B	#�B	'�B	+B	-B	.B	0!B	49B	6FB	8RB	<jB	>wB	?}B	@�B	A�B	C�B	F�B	G�B	H�B	K�B	M�B	N�B	N�B	N�B	'�B	+B	-B	.B	0!B	49B	6FB	8RB	<jB	>wB	?}B	@�B	A�B	C�B	F�B	G�B	H�B	K�B	M�B	N�B	N�B	N�B	N�B	P�B	R�B	S�B	T�B	W
B	XB	[#B	]/B	]/B	]/B	^5B	aHB	bNB	cTB	cTB	dZB	hsB	iyB	k�B	m�B	n�B	n�B	o�B	q�B	q�B	s�B	t�B	t�B	u�B	y�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�1B	�=B	�=B	�bB	�hB	�hB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�?B	�RB	�^B	�jB	B	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�HB	�HB	�HB	�TB	�TB	�ZB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
�B
�B
!�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141427                              AO  ARCAADJP                                                                    20181024141427    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141427  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141427  QCF$                G�O�G�O�G�O�4000            