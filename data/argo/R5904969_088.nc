CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:17Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141417  20181024141417  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               XA   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��$�[�1   @��%5�<@2�7KƧ��c��n��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      XA   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy�D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @0��@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB��B��B=qB ��B(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C(�C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CY��C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C���C���C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
=D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI}qDJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW}qDX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds
=Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw}qDy��D�N�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9A�9A�!A��A�A��A��A�+A�ȴA���A�!A�~�A�hsA�dZA�ffA�Q�A�K�A�9XA��A��#A���A�ȴAٮA�`BA۸RAۑhA���A���A׋DA�9XAԕ�A�%A� �A���AЁA��A�9XA͙�A��
Ȁ\A�
=A�v�A�"�A�ȴA�jA��AǶFA�VA���AŸRA�\)A�p�A��A��A�;dA��TA�
=A��^A��A��A��A�E�A��A���A��!A�p�A�XA�I�A��uA�=qA���A�ffA��9A���A�bNA�~�A�v�A�A���A���A��A�A��A���A�+A���A��A�ffA�  A��jA�"�A��A��A�jA��TA�jA��`A�-A�C�A�VA�A�VA�hsA}O�Ay�mAv1ArJAn�+Al�AkdZAi�-Af(�Aa�PAa
=A`bNA_�hA]�;A]"�A\JA[C�AZ�jAY�AY33AX��AX �AV�AR�yAR=qAQ��AOt�ANffAM��AMdZAK�AI�
AG7LAD��AC�
AC/AB��AA��A?�hA;/A:JA9ƨA9|�A9C�A8��A8Q�A7�A7�-A6�yA6  A2E�A0JA.^5A-�-A+��A(�/A&��A&bA%�7A$��A"n�A 1'Ap�A�AS�A�jAffA�7A7LA�#A��A�;AXA�A�;A"�A�9Az�Av�AbNAn�A^5AbA�FAp�AO�A+A"�A��A{AA|�A��A�A��A��A��A~�AM�A�#A
�jA	;dA��AQ�AK�A��AS�@�
=@��y@��9@��\@�S�@��/@@�p�@�D@�^5@�&�@�\)@��@�7L@�F@�5?@�O�@�x�@��@�@ޟ�@�%@�v�@�33@ա�@���@Դ9@ԣ�@ԛ�@ԓu@ӍP@��@��H@҇+@�5?@щ7@Гu@Л�@Л�@��@�S�@��T@���@͙�@�p�@͑h@́@�7L@̣�@˥�@�S�@�  @�bN@�z�@�z�@�r�@��@�"�@ʸR@ʟ�@ʟ�@�v�@��T@ɉ7@�7L@ȴ9@���@Ǿw@�K�@�ȴ@�~�@ŉ7@��`@���@ă@�I�@Ý�@���@°!@��T@�p�@��@���@��D@� �@�ƨ@�dZ@�v�@��@��@���@��D@��@�S�@��@�;d@�o@��@��R@�M�@��@�r�@�I�@�bN@�(�@�b@��m@�|�@�@�ȴ@�v�@��R@���@�^5@�$�@�/@�V@��`@��D@�9X@�Q�@��@��m@���@�dZ@�;d@��@�-@�x�@�?}@���@��`@���@�I�@�A�@�r�@�Q�@��@�^5@���@��@��D@�V@���@�S�@��@�7L@��T@�M�@���@�@�hs@��@���@�C�@���@�M�@�-@�{@���@�x�@��@��`@��@��@�9X@���@��@�33@�j@��@��@��F@��w@�S�@��y@���@�ff@�=q@�J@�hs@��@��u@�Q�@�Z@�Z@�j@�z�@��
@��\@��@��@��#@�x�@���@�j@��;@���@��
@��P@�\)@�;d@���@�n�@��@���@��7@��7@�O�@���@�Ĝ@�bN@� �@���@�\)@�
=@�~�@�$�@�@���@�x�@�p�@�V@���@���@��u@�bN@�1@���@�o@��@�E�@��^@�p�@�/@���@��D@�Z@�A�@� �@�A�@�j@�Z@�  @�C�@���@�@��@�+@��@���@��\@�~�@�V@�E�@��#@�p�@�O�@�/@��@��@�V@���@��/@�Ĝ@��@�I�@���@��@�"�@�o@�
=@�
=@�
=@��@�K�@�C�@��@�@���@��y@���@���@��+@��+@�e@}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�9A�9A�!A��A�A��A��A�+A�ȴA���A�!A�~�A�hsA�dZA�ffA�Q�A�K�A�9XA��A��#A���A�ȴAٮA�`BA۸RAۑhA���A���A׋DA�9XAԕ�A�%A� �A���AЁA��A�9XA͙�A��
Ȁ\A�
=A�v�A�"�A�ȴA�jA��AǶFA�VA���AŸRA�\)A�p�A��A��A�;dA��TA�
=A��^A��A��A��A�E�A��A���A��!A�p�A�XA�I�A��uA�=qA���A�ffA��9A���A�bNA�~�A�v�A�A���A���A��A�A��A���A�+A���A��A�ffA�  A��jA�"�A��A��A�jA��TA�jA��`A�-A�C�A�VA�A�VA�hsA}O�Ay�mAv1ArJAn�+Al�AkdZAi�-Af(�Aa�PAa
=A`bNA_�hA]�;A]"�A\JA[C�AZ�jAY�AY33AX��AX �AV�AR�yAR=qAQ��AOt�ANffAM��AMdZAK�AI�
AG7LAD��AC�
AC/AB��AA��A?�hA;/A:JA9ƨA9|�A9C�A8��A8Q�A7�A7�-A6�yA6  A2E�A0JA.^5A-�-A+��A(�/A&��A&bA%�7A$��A"n�A 1'Ap�A�AS�A�jAffA�7A7LA�#A��A�;AXA�A�;A"�A�9Az�Av�AbNAn�A^5AbA�FAp�AO�A+A"�A��A{AA|�A��A�A��A��A��A~�AM�A�#A
�jA	;dA��AQ�AK�A��AS�@�
=@��y@��9@��\@�S�@��/@@�p�@�D@�^5@�&�@�\)@��@�7L@�F@�5?@�O�@�x�@��@�@ޟ�@�%@�v�@�33@ա�@���@Դ9@ԣ�@ԛ�@ԓu@ӍP@��@��H@҇+@�5?@щ7@Гu@Л�@Л�@��@�S�@��T@���@͙�@�p�@͑h@́@�7L@̣�@˥�@�S�@�  @�bN@�z�@�z�@�r�@��@�"�@ʸR@ʟ�@ʟ�@�v�@��T@ɉ7@�7L@ȴ9@���@Ǿw@�K�@�ȴ@�~�@ŉ7@��`@���@ă@�I�@Ý�@���@°!@��T@�p�@��@���@��D@� �@�ƨ@�dZ@�v�@��@��@���@��D@��@�S�@��@�;d@�o@��@��R@�M�@��@�r�@�I�@�bN@�(�@�b@��m@�|�@�@�ȴ@�v�@��R@���@�^5@�$�@�/@�V@��`@��D@�9X@�Q�@��@��m@���@�dZ@�;d@��@�-@�x�@�?}@���@��`@���@�I�@�A�@�r�@�Q�@��@�^5@���@��@��D@�V@���@�S�@��@�7L@��T@�M�@���@�@�hs@��@���@�C�@���@�M�@�-@�{@���@�x�@��@��`@��@��@�9X@���@��@�33@�j@��@��@��F@��w@�S�@��y@���@�ff@�=q@�J@�hs@��@��u@�Q�@�Z@�Z@�j@�z�@��
@��\@��@��@��#@�x�@���@�j@��;@���@��
@��P@�\)@�;d@���@�n�@��@���@��7@��7@�O�@���@�Ĝ@�bN@� �@���@�\)@�
=@�~�@�$�@�@���@�x�@�p�@�V@���@���@��u@�bN@�1@���@�o@��@�E�@��^@�p�@�/@���@��D@�Z@�A�@� �@�A�@�j@�Z@�  @�C�@���@�@��@�+@��@���@��\@�~�@�V@�E�@��#@�p�@�O�@�/@��@��@�V@���@��/@�Ĝ@��@�I�@���@��@�"�@�o@�
=@�
=@�
=@��@�K�@�C�@��@�@���@��y@���@���@��+@��+@�e@}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
��B
��B
�hB
�bB
�oB
��B
��B
��B
��B
�%B
8RB
F�B
[#B
|�B
�#BaHB��B�uB��B�7B��B�'BŢB��B�/B�/B�mB�5B��B��B�sB�NBȴB��B��B�HB  B+B�B,B=qBVB`BBffBt�B�1B�bB�{B�bB{�Bp�BiyB[#BL�BK�BK�BJ�BM�BI�BN�BP�BK�BF�BA�BG�BK�BXB_;B`BBP�B49B�B�B��B�LB��B�PBiyB.B�B
��B
�jB
�uB
o�B
S�B
L�B
W
B
N�B
hsB
y�B
Q�B
R�B
W
B
?}B
 �B
1B	�B	�
B	�}B	�B	��B	��B	�VB	p�B	l�B	hsB	e`B	\)B	VB	P�B	K�B	H�B	E�B	@�B	=qB	9XB	2-B	#�B	�B	�B	bB	
=B	+B	B��B��B�B�yB�`B�TB�BB�5B�B��B��B��B��B��B��B��B��B��B��BɺBǮB��B�wB�}BB�qB�qB�jB�dB�XB�RB�9B�!B�B�LB�?B�9B�-B�-B�'B�9B�FB�XB�RB�RB�LB�LB�FB�FB�FB�LB�RB�dB�dB�jB�jB�jB�dB�dB�dBBɺBɺBǮBƨBÖB��B��BÖBĜB��B�jB�dB�dB�XB�!B�{B�1B�+B�1B�7B�+B�%B�+B�1B�1B�%B�7B�{B�{B�{B��B��B��B��B�9B��B�9B�LB�9B�!B�B�B�3B�^BÖBȴB��B��B�5B�B�B�B�B�B��B��B��B��B	B	B	B		7B	PB	\B	bB	hB	�B	�B	&�B	(�B	(�B	)�B	.B	33B	5?B	5?B	5?B	6FB	9XB	;dB	<jB	?}B	B�B	B�B	C�B	F�B	G�B	L�B	R�B	S�B	W
B	XB	\)B	aHB	bNB	e`B	gmB	jB	k�B	l�B	o�B	p�B	s�B	w�B	x�B	y�B	z�B	{�B	}�B	� B	� B	�B	�B	�B	�%B	�%B	�B	�%B	�%B	�=B	�DB	�DB	�DB	�DB	�7B	�=B	�=B	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�'B	�B	�B	�!B	�FB	�FB	�-B	�B	�B	�'B	�XB	�XB	�dB	�jB	�XB	�LB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�XB	�jB	�}B	��B	B	B	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�B	�)B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B

=B
DB
JB
PB
VB
VB
\B
\B
\B
VB
VB
VB
VB
PB
VB
VB
VB
VB
VB
VB
\B
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
��B
��B
�hB
�bB
�oB
��B
��B
��B
��B
�%B
8RB
F�B
[#B
|�B
�#BaHB��B�uB��B�7B��B�'BŢB��B�/B�/B�mB�5B��B��B�sB�NBȴB��B��B�HB  B+B�B,B=qBVB`BBffBt�B�1B�bB�{B�bB{�Bp�BiyB[#BL�BK�BK�BJ�BM�BI�BN�BP�BK�BF�BA�BG�BK�BXB_;B`BBP�B49B�B�B��B�LB��B�PBiyB.B�B
��B
�jB
�uB
o�B
S�B
L�B
W
B
N�B
hsB
y�B
Q�B
R�B
W
B
?}B
 �B
1B	�B	�
B	�}B	�B	��B	��B	�VB	p�B	l�B	hsB	e`B	\)B	VB	P�B	K�B	H�B	E�B	@�B	=qB	9XB	2-B	#�B	�B	�B	bB	
=B	+B	B��B��B�B�yB�`B�TB�BB�5B�B��B��B��B��B��B��B��B��B��B��BɺBǮB��B�wB�}BB�qB�qB�jB�dB�XB�RB�9B�!B�B�LB�?B�9B�-B�-B�'B�9B�FB�XB�RB�RB�LB�LB�FB�FB�FB�LB�RB�dB�dB�jB�jB�jB�dB�dB�dBBɺBɺBǮBƨBÖB��B��BÖBĜB��B�jB�dB�dB�XB�!B�{B�1B�+B�1B�7B�+B�%B�+B�1B�1B�%B�7B�{B�{B�{B��B��B��B��B�9B��B�9B�LB�9B�!B�B�B�3B�^BÖBȴB��B��B�5B�B�B�B�B�B��B��B��B��B	B	B	B		7B	PB	\B	bB	hB	�B	�B	&�B	(�B	(�B	)�B	.B	33B	5?B	5?B	5?B	6FB	9XB	;dB	<jB	?}B	B�B	B�B	C�B	F�B	G�B	L�B	R�B	S�B	W
B	XB	\)B	aHB	bNB	e`B	gmB	jB	k�B	l�B	o�B	p�B	s�B	w�B	x�B	y�B	z�B	{�B	}�B	� B	� B	�B	�B	�B	�%B	�%B	�B	�%B	�%B	�=B	�DB	�DB	�DB	�DB	�7B	�=B	�=B	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�'B	�B	�B	�!B	�FB	�FB	�-B	�B	�B	�'B	�XB	�XB	�dB	�jB	�XB	�LB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�XB	�jB	�}B	��B	B	B	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�B	�)B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B

=B
DB
JB
PB
VB
VB
\B
\B
\B
VB
VB
VB
VB
PB
VB
VB
VB
VB
VB
VB
\B
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141417                              AO  ARCAADJP                                                                    20181024141417    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141417  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141417  QCF$                G�O�G�O�G�O�0               