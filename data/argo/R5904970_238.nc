CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:49Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141549  20181024141549  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��n���1   @��  F@6�I�^5�d=�^5?}1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy��D�D)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@H��@�z�@�z�A=qA"=qAB=qAb=qA��A��A��A��A�Q�A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP(�BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2=qC4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds\Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�Dy�{D�H�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�A�A�=qA�?}A�O�A�O�A�O�A�O�A�Q�A�O�A�M�A�Q�A�VA�VA�Q�A�Q�A�VA�XA�S�A�O�A�Q�A�S�A�ZA�VA�(�A��A���A�XA�  A�bA��^A���A�x�A���A�ZA�/A�bA���A��PA�VA�33A�
=A���A�x�A�?}A���A�&�A��uA�{A���A�ZA��\A�C�A�&�A���A���A�r�A�$�A�oA���A�x�A��HA�O�A��A�l�A�I�A��A�$�A���A�A�`BA���A�t�A�  A��A�&�A�oA���A�A���A�1'A�ȴA��FA��A�M�A�C�A�\)A�;dA�+A��-A�K�A��yA�\)A�~�A��;A�VA��HA���A���A�%A�VA���A��+A���A�&�A�^5A� �A��A���A�A�
=A�33A�&�A�%A�A�dZA���A��+A�1'A��uA�~�A���A� �A
=A}\)A|r�A{��A{Ay��Ax��Aw��AwoAuAr^5Ap��Am��Ak|�AkoAj�RAg�mAe�;Ac�AaƨAax�Aa
=A
=A}\)A|r�A{��A{Ay��Ax��Aw��AwoAuAr^5Ap��Am��Ak|�AkoAj�RAg�mAe�;Ac�AaƨAax�Aa
=A`Q�A^��A\�`A[7LAY�FAX�+AV�yAU�AT��AS�mASƨASp�AQ��AP��AOG�AMK�AL9XAKAK�AI`BAG�AChsA@�`A@M�A?;dA>JA<��A;VA:VA9�A9A8�A8~�A7�7A6�!A61'A5�^A5"�A4z�A1��A.r�A,jA+S�A*^5A(��A'?}A&=qA$��A#��A#�^A#K�A"�!A!��A!O�A �A (�A&�A7LAA��A��A��Ax�Al�AdZA�Az�A1'A�;A��A�A��A�PAr�AO�A�/Az�A
=A�A33A�AZA-A�hAG�A�A�^A
��A	�TA	��A	�wA	A	+A�wAx�Al�A&�A^5Az�AbA�A��AVA33A   @�v�@��@�9X@���@�X@���@���@��@��h@�`B@���@��@�n�@�$�@��@�`B@���@�G�@���@ꗍ@�@�@��@�@蛦@�(�@�K�@��H@�5?@���@��@�`B@��`@�P@��@�ƨ@�
=@��@���@۾w@�{@�j@��@�@���@�ȴ@ѩ�@��H@�p�@���@�C�@�@�^5@ɩ�@�O�@���@���@Ȭ@��@Ǿw@���@ʰ!@˶F@�1@̋D@�K�@�7L@�Q�@�V@ģ�@�b@�j@�z�@�r�@�  @å�@�{@�9X@���@��u@��@�t�@�o@�~�@�5?@�$�@��T@�7L@�Z@��w@�E�@�O�@���@��@�|�@��!@��#@���@��7@���@��j@��u@�j@�t�@�+@�ff@�@��7@�p�@�`B@�X@�O�@�?}@�V@���@�o@��@���@���@���@�$�@�X@��`@��@�A�@��w@�C�@���@�v�@�-@�G�@���@��u@��@�+@�J@��@�V@�Q�@��@�(�@��D@���@���@��m@�K�@�dZ@��7@�S�@��R@�n�@�Ĝ@�;d@��!@�ff@�-@�@��@��#@�@��T@���@��h@�x�@�hs@���@��@�Z@���@���@���@���@��@���@���@�dZ@�K�@�;d@�33@�
=@�ȴ@��+@�v�@�V@�E�@�E�@�=q@�J@��@�O�@��@�%@���@��/@��j@�r�@��@���@���@�dZ@��!@���@�x�@�?}@�%@���@��u@�z�@�bN@�1'@���@���@�dZ@��@��H@�@���@�$�@���@�@�x�@�/@��@��`@�Ĝ@���@�ƨ@���@��T@��-@���@��h@��h@��^@�&�@��@� �@���@�  @��;@���@���@�t�@�+@��R@��!@��@y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�A�A�=qA�?}A�O�A�O�A�O�A�O�A�Q�A�O�A�M�A�Q�A�VA�VA�Q�A�Q�A�VA�XA�S�A�O�A�Q�A�S�A�ZA�VA�(�A��A���A�XA�  A�bA��^A���A�x�A���A�ZA�/A�bA���A��PA�VA�33A�
=A���A�x�A�?}A���A�&�A��uA�{A���A�ZA��\A�C�A�&�A���A���A�r�A�$�A�oA���A�x�A��HA�O�A��A�l�A�I�A��A�$�A���A�A�`BA���A�t�A�  A��A�&�A�oA���A�A���A�1'A�ȴA��FA��A�M�A�C�A�\)A�;dA�+A��-A�K�A��yA�\)A�~�A��;A�VA��HA���A���A�%A�VA���A��+A���A�&�A�^5A� �A��A���A�A�
=A�33A�&�A�%A�A�dZA���A��+A�1'A��uA�~�A���A� �A
=A}\)A|r�A{��A{Ay��Ax��Aw��AwoAuAr^5Ap��Am��Ak|�AkoAj�RAg�mAe�;Ac�AaƨAax�Aa
=A
=A}\)A|r�A{��A{Ay��Ax��Aw��AwoAuAr^5Ap��Am��Ak|�AkoAj�RAg�mAe�;Ac�AaƨAax�Aa
=A`Q�A^��A\�`A[7LAY�FAX�+AV�yAU�AT��AS�mASƨASp�AQ��AP��AOG�AMK�AL9XAKAK�AI`BAG�AChsA@�`A@M�A?;dA>JA<��A;VA:VA9�A9A8�A8~�A7�7A6�!A61'A5�^A5"�A4z�A1��A.r�A,jA+S�A*^5A(��A'?}A&=qA$��A#��A#�^A#K�A"�!A!��A!O�A �A (�A&�A7LAA��A��A��Ax�Al�AdZA�Az�A1'A�;A��A�A��A�PAr�AO�A�/Az�A
=A�A33A�AZA-A�hAG�A�A�^A
��A	�TA	��A	�wA	A	+A�wAx�Al�A&�A^5Az�AbA�A��AVA33A   @�v�@��@�9X@���@�X@���@���@��@��h@�`B@���@��@�n�@�$�@��@�`B@���@�G�@���@ꗍ@�@�@��@�@蛦@�(�@�K�@��H@�5?@���@��@�`B@��`@�P@��@�ƨ@�
=@��@���@۾w@�{@�j@��@�@���@�ȴ@ѩ�@��H@�p�@���@�C�@�@�^5@ɩ�@�O�@���@���@Ȭ@��@Ǿw@���@ʰ!@˶F@�1@̋D@�K�@�7L@�Q�@�V@ģ�@�b@�j@�z�@�r�@�  @å�@�{@�9X@���@��u@��@�t�@�o@�~�@�5?@�$�@��T@�7L@�Z@��w@�E�@�O�@���@��@�|�@��!@��#@���@��7@���@��j@��u@�j@�t�@�+@�ff@�@��7@�p�@�`B@�X@�O�@�?}@�V@���@�o@��@���@���@���@�$�@�X@��`@��@�A�@��w@�C�@���@�v�@�-@�G�@���@��u@��@�+@�J@��@�V@�Q�@��@�(�@��D@���@���@��m@�K�@�dZ@��7@�S�@��R@�n�@�Ĝ@�;d@��!@�ff@�-@�@��@��#@�@��T@���@��h@�x�@�hs@���@��@�Z@���@���@���@���@��@���@���@�dZ@�K�@�;d@�33@�
=@�ȴ@��+@�v�@�V@�E�@�E�@�=q@�J@��@�O�@��@�%@���@��/@��j@�r�@��@���@���@�dZ@��!@���@�x�@�?}@�%@���@��u@�z�@�bN@�1'@���@���@�dZ@��@��H@�@���@�$�@���@�@�x�@�/@��@��`@�Ĝ@���@�ƨ@���@��T@��-@���@��h@��h@��^@�&�@��@� �@���@�  @��;@���@���@�t�@�+@��R@��!@��@y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�5B�BB�fB�B��B��B��BB	7BVB\B\BuB�B�B�B�B �B!�B �B�B�B�BoBoB\B\BVBhBuB\BPB	7B%BB�BȴB�wB�?B�'B�!B�B��B��B��B��B�oB�PB�1B{�Bt�Bk�BZBK�BC�B:^B6FB5?B33B2-B0!B"�BuB	7BB��B��B�B�fB�B��BĜBÖB��B��B{�Bt�Bv�BbNBN�B49B0!B-B(�B�BuB%BBB
��B
�)B
��B
��B
��B
ĜB
�^B
�B
��B
��B
�hB
�1B
�%B
� B
s�B
n�B
dZB
^5B
S�B
@�B
33B
�B
B	��B	��B	�sB	�/B	��B	B	�wB�B
��B
�hB
�1B
�%B
� B
s�B
n�B
dZB
^5B
S�B
@�B
33B
�B
B	��B	��B	�sB	�/B	��B	B	�wB	�XB	�?B	�B	��B	��B	�JB	�%B	{�B	t�B	o�B	iyB	gmB	dZB	_;B	S�B	O�B	B�B	;dB	7LB	5?B	0!B	�B	VB��B��B��B�B�B�TB�BB�#B�B�B�B��B��B��B��BȴBǮB�dB�!B��B��B��B��B��B�uB�\B�DB�=B�7B�%B�B�B~�B|�Bw�Bu�Bn�Bm�Bl�Bl�Bl�Bl�Bk�Bk�BjBhsBgmBe`BcTBbNB`BB`BB[#BYBW
BS�BP�BJ�BH�BG�BF�BF�BD�BC�BB�B?}B=qB=qB<jB<jB;dB:^B8RB7LB7LB6FB49B33B2-B1'B0!B1'B2-B33B49B6FB8RB;dB>wBB�BE�BF�BF�BH�BL�BO�BN�BN�BN�BR�BVBXB[#B\)B]/B_;BaHBaHBbNBbNBbNBcTBbNBaHB`BB^5B`BB_;B^5B^5B^5B_;BaHB`BB[#BYB\)B_;B_;B_;B`BBhsBs�Bw�Bw�By�B}�B�B�B�B�+B�\B��B��B�XBBĜB��B��BǮBƨBȴB��B��B�#B�BB�mB�B�yB�mB�NB�B��B��B	  B	B	B	B	B	+B		7B	
=B	
=B	JB	hB	�B	�B	�B	�B	�B	�B	$�B	&�B	(�B	)�B	,B	0!B	2-B	;dB	@�B	A�B	A�B	B�B	C�B	D�B	E�B	H�B	O�B	T�B	W
B	XB	XB	XB	[#B	^5B	aHB	cTB	iyB	k�B	n�B	q�B	r�B	q�B	t�B	t�B	t�B	v�B	w�B	y�B	x�B	x�B	y�B	z�B	|�B	� B	�B	�B	�B	�+B	�7B	�B	{�B	x�B	y�B	s�B	n�B	k�B	jB	k�B	l�B	m�B	n�B	o�B	q�B	q�B	p�B	q�B	p�B	s�B	t�B	t�B	u�B	v�B	w�B	z�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�=B	�DB	�PB	�VB	�\B	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�9B	�3B	�FB	�LB	�XB	�XB	�XB	�XB	�^B	�jB	�dB	�XB	�FB	�?B	�?B	�FB	�LB	�XB	�dB	�^B	�^B	�jB	�qB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�5B�BB�fB�B��B��B��BB	7BVB\B\BuB�B�B�B�B �B!�B �B�B�B�BoBoB\B\BVBhBuB\BPB	7B%BB�BȴB�wB�?B�'B�!B�B��B��B��B��B�oB�PB�1B{�Bt�Bk�BZBK�BC�B:^B6FB5?B33B2-B0!B"�BuB	7BB��B��B�B�fB�B��BĜBÖB��B��B{�Bt�Bv�BbNBN�B49B0!B-B(�B�BuB%BBB
��B
�)B
��B
��B
��B
ĜB
�^B
�B
��B
��B
�hB
�1B
�%B
� B
s�B
n�B
dZB
^5B
S�B
@�B
33B
�B
B	��B	��B	�sB	�/B	��B	B	�wB�B
��B
�hB
�1B
�%B
� B
s�B
n�B
dZB
^5B
S�B
@�B
33B
�B
B	��B	��B	�sB	�/B	��B	B	�wB	�XB	�?B	�B	��B	��B	�JB	�%B	{�B	t�B	o�B	iyB	gmB	dZB	_;B	S�B	O�B	B�B	;dB	7LB	5?B	0!B	�B	VB��B��B��B�B�B�TB�BB�#B�B�B�B��B��B��B��BȴBǮB�dB�!B��B��B��B��B��B�uB�\B�DB�=B�7B�%B�B�B~�B|�Bw�Bu�Bn�Bm�Bl�Bl�Bl�Bl�Bk�Bk�BjBhsBgmBe`BcTBbNB`BB`BB[#BYBW
BS�BP�BJ�BH�BG�BF�BF�BD�BC�BB�B?}B=qB=qB<jB<jB;dB:^B8RB7LB7LB6FB49B33B2-B1'B0!B1'B2-B33B49B6FB8RB;dB>wBB�BE�BF�BF�BH�BL�BO�BN�BN�BN�BR�BVBXB[#B\)B]/B_;BaHBaHBbNBbNBbNBcTBbNBaHB`BB^5B`BB_;B^5B^5B^5B_;BaHB`BB[#BYB\)B_;B_;B_;B`BBhsBs�Bw�Bw�By�B}�B�B�B�B�+B�\B��B��B�XBBĜB��B��BǮBƨBȴB��B��B�#B�BB�mB�B�yB�mB�NB�B��B��B	  B	B	B	B	B	+B		7B	
=B	
=B	JB	hB	�B	�B	�B	�B	�B	�B	$�B	&�B	(�B	)�B	,B	0!B	2-B	;dB	@�B	A�B	A�B	B�B	C�B	D�B	E�B	H�B	O�B	T�B	W
B	XB	XB	XB	[#B	^5B	aHB	cTB	iyB	k�B	n�B	q�B	r�B	q�B	t�B	t�B	t�B	v�B	w�B	y�B	x�B	x�B	y�B	z�B	|�B	� B	�B	�B	�B	�+B	�7B	�B	{�B	x�B	y�B	s�B	n�B	k�B	jB	k�B	l�B	m�B	n�B	o�B	q�B	q�B	p�B	q�B	p�B	s�B	t�B	t�B	u�B	v�B	w�B	z�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�=B	�DB	�PB	�VB	�\B	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�9B	�3B	�FB	�LB	�XB	�XB	�XB	�XB	�^B	�jB	�dB	�XB	�FB	�?B	�?B	�FB	�LB	�XB	�dB	�^B	�^B	�jB	�qB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141549                              AO  ARCAADJP                                                                    20181024141549    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141549  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141549  QCF$                G�O�G�O�G�O�4000            