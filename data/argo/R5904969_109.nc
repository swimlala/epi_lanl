CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:21Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024141421  20181024141421  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               mA   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��d�4�1   @��effy@3�$�/��c��vȴ91   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      mA   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy��D�ND���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @C�
@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh��Bp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C(�C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	
=D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2}qD3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�=DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da}qDb�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�=Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�qDy�\D�P D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aݣ�Aݥ�Aݝ�Aݛ�Aݛ�Aݧ�AݮAݧ�Aݲ-AݮAݲ-Aݺ^A�Aݺ^Aݺ^A�ȴA���A���A���A���A���A���A���A���A�ȴA���A���A��A��
A��A��A���A�n�A��`A�jA�C�AڅA� �A��/A�oA�
=A�S�A֡�A�=qA� �A�^5A�A�A��A���A�7LAк^A��AϓuA��A�G�A�dZA�`BA�z�A��Aȝ�A��
A�A�{A���A� �A���Aº^A�`BA��`A�/A��TA��A�`BA��RA���A���A�  A��yA�E�A�I�A��jA���A���A��A��TA���A�+A�dZA��7A�O�A�`BA�x�A���A�
=A��A���A�1'A���A��A��A��A��^A��/A���A��A���A�E�A��7A�~�A�r�A�l�A���A�JA�XA��;A��A�n�A��A�l�A��A�FA|�!Ay�wAx��Ax{Av�yAuC�As&�Apz�Am�
AhQ�Ae|�A`�RAZ��AYG�AX=qATv�AQ��APffAN��AMAL��AL(�AK�AJbAHM�AF��ADbAA&�A>jA<�+A:�A9�A81A6��A5/A4VA3oA2�A/�A/�A.��A-"�A+O�A*�uA)��A(�+A&��A%�TA%hsA#/A"-A!oA VA �A��AG�A�
A�`AAhsA�/A(�AƨA�+Av�A�^Av�AƨAoAx�AJA��AdZAbNAS�A�A
bNA	VAt�AVA�A�AoA��A��AAO�A`BA&�A ZA 9X@�l�@���@���@�p�@��@���@�V@���@�X@��@�x�@�5?@�?}@�Q�@��@���@�@�\@�@���@�@��@�@�V@�P@��@��/@�Z@��m@��@�ƨ@�r�@���@�b@�1@�Z@� �@�@�"�@�hs@ߕ�@��@܋D@�bN@�I�@��@�M�@��@�r�@�1'@��@�+@��#@��`@��@ӍP@�dZ@��@�v�@с@�G�@�G�@�j@�o@��#@�?}@̣�@�1'@���@�
=@�v�@�J@�@�O�@ȃ@�b@��
@�ƨ@�|�@Ə\@��@ēu@Ý�@�S�@�@�n�@��#@���@�`B@�b@��F@��w@���@�t�@���@��@��^@��7@��@�9X@��@��@��P@�
=@���@�v�@���@�?}@��@���@��
@��@�\)@�
=@��!@���@�n�@�V@�E�@�5?@�{@�?}@���@��D@�bN@�b@���@��m@���@�S�@�;d@�33@��!@�$�@�?}@��7@��h@�7L@��j@�Q�@��@��;@��F@���@�|�@�S�@�33@�@�ȴ@�~�@�ff@�5?@��@�?}@��@�z�@��m@�+@�-@���@���@���@��u@�Q�@�  @�ƨ@���@�33@�ȴ@�=q@�@��-@��7@�7L@�hs@�hs@�p�@�hs@�O�@��@��@���@���@��9@��u@�Z@��@��y@�5?@��7@�/@��j@�Z@�ƨ@��P@�\)@�+@�"�@�|�@���@��m@���@��m@��w@���@�t�@�S�@�C�@�33@�+@�@�^5@�p�@�7L@��`@�Q�@�1@��w@���@�t�@�\)@�+@���@�ȴ@�v�@�M�@��@��T@���@��@���@�%@�z�@���@��m@���@�ƨ@��w@�|�@�@���@�x�@�X@�/@��@�(�@�1@��@��;@�ƨ@��P@��@�|�@�t�@�+@���@��@��y@��@���@���@���@��-@��h@�hs@�X@��@��@��`@��@���@���@���@��/@�Ĝ@���@��@��@�z�@�(�@��@��@�"�@���@��g@�6@xN�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aݣ�Aݥ�Aݝ�Aݛ�Aݛ�Aݧ�AݮAݧ�Aݲ-AݮAݲ-Aݺ^A�Aݺ^Aݺ^A�ȴA���A���A���A���A���A���A���A���A�ȴA���A���A��A��
A��A��A���A�n�A��`A�jA�C�AڅA� �A��/A�oA�
=A�S�A֡�A�=qA� �A�^5A�A�A��A���A�7LAк^A��AϓuA��A�G�A�dZA�`BA�z�A��Aȝ�A��
A�A�{A���A� �A���Aº^A�`BA��`A�/A��TA��A�`BA��RA���A���A�  A��yA�E�A�I�A��jA���A���A��A��TA���A�+A�dZA��7A�O�A�`BA�x�A���A�
=A��A���A�1'A���A��A��A��A��^A��/A���A��A���A�E�A��7A�~�A�r�A�l�A���A�JA�XA��;A��A�n�A��A�l�A��A�FA|�!Ay�wAx��Ax{Av�yAuC�As&�Apz�Am�
AhQ�Ae|�A`�RAZ��AYG�AX=qATv�AQ��APffAN��AMAL��AL(�AK�AJbAHM�AF��ADbAA&�A>jA<�+A:�A9�A81A6��A5/A4VA3oA2�A/�A/�A.��A-"�A+O�A*�uA)��A(�+A&��A%�TA%hsA#/A"-A!oA VA �A��AG�A�
A�`AAhsA�/A(�AƨA�+Av�A�^Av�AƨAoAx�AJA��AdZAbNAS�A�A
bNA	VAt�AVA�A�AoA��A��AAO�A`BA&�A ZA 9X@�l�@���@���@�p�@��@���@�V@���@�X@��@�x�@�5?@�?}@�Q�@��@���@�@�\@�@���@�@��@�@�V@�P@��@��/@�Z@��m@��@�ƨ@�r�@���@�b@�1@�Z@� �@�@�"�@�hs@ߕ�@��@܋D@�bN@�I�@��@�M�@��@�r�@�1'@��@�+@��#@��`@��@ӍP@�dZ@��@�v�@с@�G�@�G�@�j@�o@��#@�?}@̣�@�1'@���@�
=@�v�@�J@�@�O�@ȃ@�b@��
@�ƨ@�|�@Ə\@��@ēu@Ý�@�S�@�@�n�@��#@���@�`B@�b@��F@��w@���@�t�@���@��@��^@��7@��@�9X@��@��@��P@�
=@���@�v�@���@�?}@��@���@��
@��@�\)@�
=@��!@���@�n�@�V@�E�@�5?@�{@�?}@���@��D@�bN@�b@���@��m@���@�S�@�;d@�33@��!@�$�@�?}@��7@��h@�7L@��j@�Q�@��@��;@��F@���@�|�@�S�@�33@�@�ȴ@�~�@�ff@�5?@��@�?}@��@�z�@��m@�+@�-@���@���@���@��u@�Q�@�  @�ƨ@���@�33@�ȴ@�=q@�@��-@��7@�7L@�hs@�hs@�p�@�hs@�O�@��@��@���@���@��9@��u@�Z@��@��y@�5?@��7@�/@��j@�Z@�ƨ@��P@�\)@�+@�"�@�|�@���@��m@���@��m@��w@���@�t�@�S�@�C�@�33@�+@�@�^5@�p�@�7L@��`@�Q�@�1@��w@���@�t�@�\)@�+@���@�ȴ@�v�@�M�@��@��T@���@��@���@�%@�z�@���@��m@���@�ƨ@��w@�|�@�@���@�x�@�X@�/@��@�(�@�1@��@��;@�ƨ@��P@��@�|�@�t�@�+@���@��@��y@��@���@���@���@��-@��h@�hs@�X@��@��@��`@��@���@���@���@��/@�Ĝ@���@��@��@�z�@�(�@��@��@�"�@���@��g@�6@xN�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
F�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
D�B
E�B
E�B
F�B
F�B
D�B
E�B
E�B
F�B
E�B
E�B
L�B
m�B
�oB
��B
��B
�uB
�hB
�{B
��B
��B
�B
��B
�HB
��BuB�B�B{BbB#�B6FBB�BR�Bn�Bs�B{�B�B��B��B�-B�^BǮB��B�B��B��B��BB�B�B�B(�B6FB8RBC�BJ�BI�BH�BG�B>wB6FB2-B)�B%�B�BuB\BJBJBuBVBB��B��BB�qB�FB�?B�-B��B��B�oBz�BXB=qB,B"�B�B
=B
�HB
��B
�B
��B
� B
u�B
o�B
VB
E�B
9XB
1'B
%�B
hB	��B	��B	�B	�yB	�/B	��B	�qB	�B	�PB	x�B	e`B	@�B	49B	0!B	�B	hB	DB	+B	B��B��B��B�B�yB�TB�B��B��B��BȴBƨBĜBB�wB�jB�LB�XB�LB�3B�'B�?B�jB�dB�qB�qB��B�wB�wBB��BBB��BBBĜBŢBȴBȴBǮBǮBƨBƨBĜBǮB��B��B��BɺBĜBÖBB�}B�wB�LB�'B�B��B��B�B��B��B��B�{B�bB�{B��B��B��B��B��B��B��B��B��B��B�XB�qB�^B�XBȴB�)B�5B�5B�)B�B�B�#B�5B�5B�5B�/B�)B�#B�;B�sB�fB�ZB�ZB�`B�mB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	1B	
=B	JB	DB	hB	�B	{B	uB	�B	�B	�B	�B	�B	$�B	'�B	(�B	)�B	,B	/B	2-B	33B	33B	49B	6FB	9XB	@�B	D�B	F�B	H�B	K�B	O�B	P�B	Q�B	[#B	\)B	\)B	\)B	]/B	aHB	e`B	ffB	ffB	iyB	n�B	n�B	p�B	r�B	u�B	v�B	v�B	u�B	v�B	x�B	x�B	x�B	x�B	w�B	x�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�?B	�LB	�LB	�LB	�LB	�LB	�RB	�RB	�RB	�LB	�LB	�RB	�dB	�^B	�^B	�XB	�RB	�RB	�^B	�}B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�5B	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
�B
B
 �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
F�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
D�B
E�B
E�B
F�B
F�B
D�B
E�B
E�B
F�B
E�B
E�B
L�B
m�B
�oB
��B
��B
�uB
�hB
�{B
��B
��B
�B
��B
�HB
��BuB�B�B{BbB#�B6FBB�BR�Bn�Bs�B{�B�B��B��B�-B�^BǮB��B�B��B��B��BB�B�B�B(�B6FB8RBC�BJ�BI�BH�BG�B>wB6FB2-B)�B%�B�BuB\BJBJBuBVBB��B��BB�qB�FB�?B�-B��B��B�oBz�BXB=qB,B"�B�B
=B
�HB
��B
�B
��B
� B
u�B
o�B
VB
E�B
9XB
1'B
%�B
hB	��B	��B	�B	�yB	�/B	��B	�qB	�B	�PB	x�B	e`B	@�B	49B	0!B	�B	hB	DB	+B	B��B��B��B�B�yB�TB�B��B��B��BȴBƨBĜBB�wB�jB�LB�XB�LB�3B�'B�?B�jB�dB�qB�qB��B�wB�wBB��BBB��BBBĜBŢBȴBȴBǮBǮBƨBƨBĜBǮB��B��B��BɺBĜBÖBB�}B�wB�LB�'B�B��B��B�B��B��B��B�{B�bB�{B��B��B��B��B��B��B��B��B��B��B�XB�qB�^B�XBȴB�)B�5B�5B�)B�B�B�#B�5B�5B�5B�/B�)B�#B�;B�sB�fB�ZB�ZB�`B�mB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	1B	
=B	JB	DB	hB	�B	{B	uB	�B	�B	�B	�B	�B	$�B	'�B	(�B	)�B	,B	/B	2-B	33B	33B	49B	6FB	9XB	@�B	D�B	F�B	H�B	K�B	O�B	P�B	Q�B	[#B	\)B	\)B	\)B	]/B	aHB	e`B	ffB	ffB	iyB	n�B	n�B	p�B	r�B	u�B	v�B	v�B	u�B	v�B	x�B	x�B	x�B	x�B	w�B	x�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�?B	�LB	�LB	�LB	�LB	�LB	�RB	�RB	�RB	�LB	�LB	�RB	�dB	�^B	�^B	�XB	�RB	�RB	�^B	�}B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�5B	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
�B
B
 �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141421                              AO  ARCAADJP                                                                    20181024141421    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141421  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141421  QCF$                G�O�G�O�G�O�0               