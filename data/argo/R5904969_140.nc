CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:28Z creation      
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
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    L�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  N�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  W   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  a�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    j(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  lH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  t�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    }8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20181024141428  20181024141428  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��$���1   @��%W:۬@3�����c��-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�T{11111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111  @=p�@}p�@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�G�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh��Bp=qBw�B��A�z�A�G�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh��Bp=qBw�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CO��CR\CT\CV\CX(�CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#
=D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP
=DP��DQ�DQ��DR�DR}qDS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ
=DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�
Dy��D�Vg11111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�I�A�dZA�dZA�dZA�ffA�dZA�`BA�`BA�^5A�dZA�^5A�bNA�dZA�hsA�hsA�hsA�n�A�n�A�p�A�r�A�r�A�jA�ffA�hsA�hsA�O�A��yA�\)A�M�AЗ�A��A�dZA�^5A�bNA�dZA�hsA�hsA�hsA�n�A�n�A�p�A�r�A�r�A�jA�ffA�hsA�hsA�O�A��yA�\)A�M�AЗ�A��A�=qA��TAˑhA�-Aʡ�A�-A��
A��AȶFA�VAƧ�A���A�1Aę�A���A7A��^A��^A��A���A���A��PA�r�A�7LA�XA��HA�{A�jA��
A��TA�XA�-A�l�A��^A�bNA��A��A��A��A��A�bA��+A�ȴA��`A���A��hA�A��DA���A�A�\)A�G�A���A��HA�jA�O�A��7A�9XA�G�A���A��A��yA�r�A��`A��RA��7A�;dA��A��;A��\A��-A�jA|��AzZAu
=As�Asx�As%Aq�hAn$�AkoAi?}Ah=qAc`BAa�TA`��A`A^Q�A[�AZA�AV�AT �AO�
AM�AI��AEhsABJA?�
A?�A?�PA?�7A?�PA?�7A?K�A=�TA;t�A;S�A;K�A:ĜA8��A6n�A5�mA5|�A5G�A4bNA17LA/��A.ffA-�TA-S�A,�jA,M�A*�yA* �A)�
A't�A%�A!A��A�#AȴA�A-A��A(�A�FA?}Az�AA�AbA"�AQ�AƨAl�A�A�mA/AE�Ap�A��AE�A��A
=A	��A=qA��AA~�A�A��A��Av�A �AƨA �A ��A V@���@�+@�@�(�@���@�ff@�"�@��@��@�@��y@�ff@�&�@�I�@�w@��@䛦@�x�@��@�1@�h@� �@��@�+@���@��@�^5@�Q�@�S�@���@��#@��@؛�@ش9@�Ĝ@؋D@�(�@ו�@�K�@��@���@���@ָR@�M�@��#@�O�@Ӿw@�ȴ@�-@Ѓ@�t�@�@�X@�S�@��H@�M�@ə�@���@�9X@�ƨ@Ǖ�@�K�@�@�@�X@��@ċD@�  @�dZ@�@�M�@�{@��@���@���@��@��9@��j@��9@���@�&�@�X@�x�@��^@���@��!@���@�Z@���@�|�@��/@�l�@��@��\@��+@�n�@�=q@��@��@�@�p�@��/@��D@�Z@��;@�dZ@�33@��@���@��\@�ff@��@���@��-@���@�X@���@��@�1'@���@��P@�+@��@��@�~�@���@��@�%@��/@���@��@�\)@��@��@�=q@���@��@�O�@��@��`@���@���@��
@���@�|�@�S�@�;d@�@��!@���@�l�@��@��F@�S�@��P@�ƨ@�|�@�o@��H@���@�ȴ@��!@�^5@���@��-@���@�hs@�?}@�/@�V@�z�@���@���@���@�n�@�ff@�ff@�V@�E�@�E�@�=q@��@��@��T@��@�-@��R@�+@�S�@��y@���@�@�"�@�"�@��y@���@���@�E�@��T@��#@���@�X@�j@�;d@�-@�p�@��@��@��@�r�@��@��F@���@��F@���@��@�K�@���@�ff@�J@�@��7@�`B@�7L@�V@��`@��`@��`@��`@��`@��`@��/@���@��j@���@�j@�(�@��@�;d@�V@���@���@���@�x�@�X@�?}@�&�@��@�Ĝ@��@��
@��F@��@��P@���@���@�x�@�hs@�hs@�`B@�?}@���@���@��u@��@�bN@�bN@�1'@��@�b@�  @��@��
@���@�t�@�;d@��y@��@��h@�O�@�7L@�&�@�%@��`@��9@��u@��@�bN@�bN@�1'@��@�b@�  @��@��
@���@�t�@�;d@��y@��@��h@�O�@�7L@�&�@�%@��`@��9@�A�@��w@��@���@���@�~�@�v�@�~�@�v�@�n�@�ff@�^5@��a@}�11111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�I�A�dZA�dZA�dZA�ffA�dZA�`BA�`BA�^5A�dZA�^5A�bNA�dZA�hsA�hsA�hsA�n�A�n�A�p�A�r�A�r�A�jA�ffA�hsA�hsA�O�A��yA�\)A�M�AЗ�A��A�dZA�^5A�bNA�dZA�hsA�hsA�hsA�n�A�n�A�p�A�r�A�r�A�jA�ffA�hsA�hsA�O�A��yA�\)A�M�AЗ�A��A�=qA��TAˑhA�-Aʡ�A�-A��
A��AȶFA�VAƧ�A���A�1Aę�A���A7A��^A��^A��A���A���A��PA�r�A�7LA�XA��HA�{A�jA��
A��TA�XA�-A�l�A��^A�bNA��A��A��A��A��A�bA��+A�ȴA��`A���A��hA�A��DA���A�A�\)A�G�A���A��HA�jA�O�A��7A�9XA�G�A���A��A��yA�r�A��`A��RA��7A�;dA��A��;A��\A��-A�jA|��AzZAu
=As�Asx�As%Aq�hAn$�AkoAi?}Ah=qAc`BAa�TA`��A`A^Q�A[�AZA�AV�AT �AO�
AM�AI��AEhsABJA?�
A?�A?�PA?�7A?�PA?�7A?K�A=�TA;t�A;S�A;K�A:ĜA8��A6n�A5�mA5|�A5G�A4bNA17LA/��A.ffA-�TA-S�A,�jA,M�A*�yA* �A)�
A't�A%�A!A��A�#AȴA�A-A��A(�A�FA?}Az�AA�AbA"�AQ�AƨAl�A�A�mA/AE�Ap�A��AE�A��A
=A	��A=qA��AA~�A�A��A��Av�A �AƨA �A ��A V@���@�+@�@�(�@���@�ff@�"�@��@��@�@��y@�ff@�&�@�I�@�w@��@䛦@�x�@��@�1@�h@� �@��@�+@���@��@�^5@�Q�@�S�@���@��#@��@؛�@ش9@�Ĝ@؋D@�(�@ו�@�K�@��@���@���@ָR@�M�@��#@�O�@Ӿw@�ȴ@�-@Ѓ@�t�@�@�X@�S�@��H@�M�@ə�@���@�9X@�ƨ@Ǖ�@�K�@�@�@�X@��@ċD@�  @�dZ@�@�M�@�{@��@���@���@��@��9@��j@��9@���@�&�@�X@�x�@��^@���@��!@���@�Z@���@�|�@��/@�l�@��@��\@��+@�n�@�=q@��@��@�@�p�@��/@��D@�Z@��;@�dZ@�33@��@���@��\@�ff@��@���@��-@���@�X@���@��@�1'@���@��P@�+@��@��@�~�@���@��@�%@��/@���@��@�\)@��@��@�=q@���@��@�O�@��@��`@���@���@��
@���@�|�@�S�@�;d@�@��!@���@�l�@��@��F@�S�@��P@�ƨ@�|�@�o@��H@���@�ȴ@��!@�^5@���@��-@���@�hs@�?}@�/@�V@�z�@���@���@���@�n�@�ff@�ff@�V@�E�@�E�@�=q@��@��@��T@��@�-@��R@�+@�S�@��y@���@�@�"�@�"�@��y@���@���@�E�@��T@��#@���@�X@�j@�;d@�-@�p�@��@��@��@�r�@��@��F@���@��F@���@��@�K�@���@�ff@�J@�@��7@�`B@�7L@�V@��`@��`@��`@��`@��`@��`@��/@���@��j@���@�j@�(�@��@�;d@�V@���@���@���@�x�@�X@�?}@�&�@��@�Ĝ@��@��
@��F@��@��P@���@���@�x�@�hs@�hs@�`B@�?}@���@���@��u@��@�bN@�bN@�1'@��@�b@�  @��@��
@���@�t�@�;d@��y@��@��h@�O�@�7L@�&�@�%@��`@��9@��u@��@�bN@�bN@�1'@��@�b@�  @��@��
@���@�t�@�;d@��y@��@��h@�O�@�7L@�&�@�%@��`@��9@�A�@��w@��@���@���@�~�@�v�@�~�@�v�@�n�@�ff@�^5@��a@}�11111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B(�B(�B'�B(�B(�B'�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B+B+B.B2-B9XBjB�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B+B+B.B2-B9XBjB�bB��B��B��B��B��B��B��B��B��B�'B��B�fB��B��B��B\B<jBO�BXBYB[#B^5BdZBgmBn�B�By�Bw�Bu�Bo�BbNB_;BYBW
BVBS�BQ�BM�BG�B:^B.B�BuBoBJBJB1BB�#B�B�{Bu�BgmBJ�B)�BhBB
��B
�B
�B
�!B
�B
n�B
gmB
e`B
cTB
`BB
VB
A�B
<jB
1'B
!�B

=B	��B	�ZB	�
B	��B	��B	��B	�^B	��B	��B	��B	�B	u�B	p�B	jB	aHB	XB	J�B	>wB	.B	�B	bB��B�B�HB��B��B��B��B��B��B��B��BĜBB��B�}B�dB�LB�FB�?B�3B�'B�3B�LB�^B�^B�^B�XB�XB�qB�jB�jB�jB�wBÖB��B��B�wB�wB�qB�XB�LB�FB�?B�9B�3B�-B�3B�3B�-B�-B�-B�9B�FB�XB�XB�RB�dB�qB�wB�qB�jB�dB�dB�dB�jB�qB�qB�wB�wB��BĜBĜBĜBĜBÖBBĜBƨBŢBŢBBŢBȴB��B��B��B��BȴBĜBÖB��B��B��B�
B��B��B�#B�/B�5B�5B�B�B�
B��B��B�B�/B�;B�;B�;B�HB�TB�ZB�`B�sB�sB�mB�mB�mB�mB�ZB�NB�BB�;B�`B�mB�yB�sB�yB�B�B�B�B�B�B�B��B��B	B	B	B	B	%B	1B	
=B	
=B	
=B	PB	hB	oB	{B	�B	�B	�B	�B	�B	#�B	)�B	'�B	)�B	,B	-B	0!B	33B	33B	33B	5?B	6FB	6FB	8RB	9XB	:^B	;dB	>wB	@�B	A�B	A�B	E�B	I�B	M�B	T�B	VB	W
B	W
B	YB	ZB	\)B	\)B	]/B	`BB	aHB	aHB	bNB	ffB	iyB	iyB	iyB	iyB	iyB	k�B	m�B	m�B	m�B	q�B	r�B	t�B	v�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�DB	�JB	�VB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�3B	�3B	�9B	�9B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�jB	��B	��B	��B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�)B	�)B	�#B	�#B	�B	�B	�
B	�B	�)B	�5B	�;B	�BB	�NB	�NB	�TB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+BaHB
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
^B
�11111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111  B(�B(�B'�B(�B(�B'�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B+B+B.B2-B9XBjB�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B+B+B.B2-B9XBjB�bB��B��B��B��B��B��B��B��B��B�'B��B�fB��B��B��B\B<jBO�BXBYB[#B^5BdZBgmBn�B�By�Bw�Bu�Bo�BbNB_;BYBW
BVBS�BQ�BM�BG�B:^B.B�BuBoBJBJB1BB�#B�B�{Bu�BgmBJ�B)�BhBB
��B
�B
�B
�!B
�B
n�B
gmB
e`B
cTB
`BB
VB
A�B
<jB
1'B
!�B

=B	��B	�ZB	�
B	��B	��B	��B	�^B	��B	��B	��B	�B	u�B	p�B	jB	aHB	XB	J�B	>wB	.B	�B	bB��B�B�HB��B��B��B��B��B��B��B��BĜBB��B�}B�dB�LB�FB�?B�3B�'B�3B�LB�^B�^B�^B�XB�XB�qB�jB�jB�jB�wBÖB��B��B�wB�wB�qB�XB�LB�FB�?B�9B�3B�-B�3B�3B�-B�-B�-B�9B�FB�XB�XB�RB�dB�qB�wB�qB�jB�dB�dB�dB�jB�qB�qB�wB�wB��BĜBĜBĜBĜBÖBBĜBƨBŢBŢBBŢBȴB��B��B��B��BȴBĜBÖB��B��B��B�
B��B��B�#B�/B�5B�5B�B�B�
B��B��B�B�/B�;B�;B�;B�HB�TB�ZB�`B�sB�sB�mB�mB�mB�mB�ZB�NB�BB�;B�`B�mB�yB�sB�yB�B�B�B�B�B�B�B��B��B	B	B	B	B	%B	1B	
=B	
=B	
=B	PB	hB	oB	{B	�B	�B	�B	�B	�B	#�B	)�B	'�B	)�B	,B	-B	0!B	33B	33B	33B	5?B	6FB	6FB	8RB	9XB	:^B	;dB	>wB	@�B	A�B	A�B	E�B	I�B	M�B	T�B	VB	W
B	W
B	YB	ZB	\)B	\)B	]/B	`BB	aHB	aHB	bNB	ffB	iyB	iyB	iyB	iyB	iyB	k�B	m�B	m�B	m�B	q�B	r�B	t�B	v�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�DB	�JB	�VB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�3B	�3B	�9B	�9B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�jB	��B	��B	��B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�)B	�)B	�#B	�#B	�B	�B	�
B	�B	�)B	�5B	�;B	�BB	�NB	�NB	�TB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+BaHB
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
^B
�11111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141428                              AO  ARCAADJP                                                                    20181024141428    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141428  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141428  QCF$                G�O�G�O�G�O�4000            