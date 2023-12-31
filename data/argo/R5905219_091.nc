CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-10-12T00:39:10Z creation;2020-10-12T00:39:13Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20201012003910  20201012005457  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               [A   JA                                  2B  A   APEX                            7906                            051216                          846 @�>�~�S1   @�>�+�d�@4ě��S��dm�E��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B?��BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C�C  C�fC  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D��Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|fD|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D3D��3D�  D�@ DÀ Dü�D�  D�@ Dă3D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D���D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�<�D�|�D�� D�3D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�3D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�
>A�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BA�RBJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B��)B�\B�\C ��C��C��C��C��C
��C��C��C�HC��CnC��C��C��C��C��C �HC"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`�HCb��Cd��Cf��ChnCj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D�D��D!�D��D!�D��D�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE(RDE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP(RDP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT�RDU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh�RDi(RDi��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw�RDx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|(RD|��D}!�D}�RD~!�D~��D!�D��D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�T)D���D���D��D�P�D���D���D��D�P�D���D��)D�)D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D�)D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D�)D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D��)D�)D�T)D)D��)D��D�P�DÐ�D���D��D�P�DĔ)D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�M�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D��)D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�M�Dލ�D���D�)D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�M�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D��)D��D�M�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D�)D���D��D�M�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�jA�z�A��A��#A�XA�$�A�ƨA�ĜA�ĜA߃A�VAܴ9A؍PA�^5A�O�A��
A�ZA�5?A�A�A˴9A�  A�jA��A�ȴAÏ\A¾wA�O�A���A�XA�r�A�XA�l�A��wA��-A�O�A�A�A��A�S�A�1'A��RA�K�A��A�t�A�S�A��/A�O�A�;dA�A�A��9A�z�A�7LA��!A��A�$�A�hsA��A���A���A�7LA��A���A�A�A�M�A�`BA��A��#A���A�(�A��A�%A��uA��A��FA�{A�A���A�&�A��`A���A��/A�%A��TA���A�ZA�=qA���A��TA�{A��^AA}
=A{�Ayx�Ax�`Ax~�AwG�At��ArZAop�Am|�Ak��Ai�Ai�Ag�^Af��Af��Ae`BAc�TAb��Aa��A_�-A^�uA]A\��A\{AY��AWXAU�hAU�PAU\)AR{AOAN~�AMƨALr�AH�yAF��ACC�A@~�A?+A>ffA=�7A;�^A:9XA9/A7�A6�9A5A2�jA2A0��A/VA-O�A,M�A+33A)x�A(��A(A&ȴA%��A%�A$�uA#�A#VA"�A"��A"�A!�
A �9A =qA�A33A��A�mA/A�#A;dAAĜA%Ax�AM�A1'A�A�A�A��AhsA�`A��A5?A�A��A(�A7LAffA^5AbA�TA`BA
^5A	%AE�AƨA��AVA��A+A��AG�A �R@�M�@�&�@�"�@�n�@�{@�=q@�~�@�$�@��^@���@�  @�@�p�@��@��@�v�@�?}@��;@��@�D@�K�@��H@�~�@�V@�ƨ@�ȴ@�5?@���@ݙ�@�p�@܋D@۶F@��@�J@�X@أ�@ו�@֟�@��@��@�9X@��
@��@�{@��@�1@�33@��@͉7@��`@� �@�~�@Ɂ@ȼj@�j@��@Ǯ@��@�J@�O�@�%@�j@�1@Õ�@�n�@�/@��u@��@���@��P@�l�@�@�M�@��@�@���@��@�G�@��/@�z�@��@�t�@���@���@�7L@���@�1@�  @�(�@���@�ȴ@�5?@���@�X@��@���@�r�@��w@�\)@��@��H@���@�^5@�E�@�-@���@��h@��@��@���@��@�b@��P@�o@��@�ȴ@�v�@��@���@���@�{@��@�O�@���@���@���@�\)@�C�@�@��y@���@�~�@��@��T@�M�@�5?@�M�@���@��@�V@�$�@�^5@��R@��H@��!@�M�@�$�@�M�@��@�@���@���@��h@�7L@�/@��`@��@���@���@��7@�`B@�&�@�`B@�`B@���@���@��#@�@�@��h@��@�p�@��9@��@�K�@�\)@�ȴ@��@���@��#@��-@���@��h@�`B@��@�Q�@��@��m@��F@��@���@�S�@��H@���@�n�@���@�-@���@�V@�z�@���@��w@��P@�~�@�$�@��#@��-@���@�7L@��@��@�j@�Q�@�Q�@���@�@�5?@�$�@���@��#@���@�x�@�p�@�O�@��@��/@���@�r�@�I�@�1'@��@���@��P@�;d@�"�@��y@���@��\@�E�@���@��-@�G�@��@�%@��@���@��@�r�@�1'@���@���@���@���@�|�@�S�@�+@��@�
=@��R@�@�`B@�%@��/@���@���@��u@�j@�(�@���@��w@�C�@���@��!@�~�@�=q@���@��h@��@�hs@�%@���@���@��@�I�@�b@�ƨ@�33@��H@��+@�M�@��#@���@�O�@��@�Ĝ@�j@��;@��@�dZ@�"�@�@�ȴ@�n�@�-@��@�@�hs@�?}@�bN@��
@��F@���@���@�33@��@��R@��+@�-@��#@���@�x�@�O�@�/@�&�@�&�@��@�V@��@�Ĝ@��@�1'@��@�1@�1@�@��@�P@\)@~��@~�R@~ff@~@|��@|�D@|j@|Z@|9X@{�m@{��@{�@{t�@{33@z�!@z=q@y��@y��@y�^@y�7@y&�@y�@x��@x�9@xbN@xb@w�;@w�P@wK�@w\)@w\)@wK�@w;d@w;d@v�y@v��@vv�@v$�@u@u�@uV@t�/@t�/@t�@tI�@s�m@s�@s"�@r��@r=q@q��@qx�@q&�@p�@o��@o\)@o\)@o+@n�@n�y@nv�@m�@m��@m�@m`B@m?}@mV@l�j@l�@k��@k�
@kƨ@k�F@ko@j��@jM�@i��@i&�@h��@g�;@gK�@f�@f��@fE�@e@eO�@d��@d��@d�j@d��@d�D@dz�@d9X@cS�@c@b��@b~�@b~�@bM�@b=q@b�@bJ@a��@ax�@`Q�@`  @_�@_�P@_
=@^�R@^v�@^@]��@]O�@]V@\��@\z�@\�@[ƨ@[t�@Z��@Y�@Yx�@YX@YG�@Y7L@Y&�@X�`@Xr�@W�;@W\)@V��@V�R@Vv�@VV@V$�@U�-@T��@Tj@S�
@S��@SC�@R�@R�!@R�\@R=q@Q��@Qhs@P�`@P�@P �@O�;@O�@O�@N�+@NE�@N{@M�@M@Mp�@MV@L��@LZ@L(�@Kƨ@KdZ@KC�@K@J�\@J=q@I��@I��@I�7@IG�@I%@HĜ@H�u@HbN@HA�@H �@G��@GK�@G+@Fv�@F5?@E�T@E�-@E/@D��@D�/@D�@Dz�@DI�@D(�@C�m@C�F@C��@Ct�@CC�@B�H@B�!@B��@B-@A�#@A�7@AG�@A�@@�9@@r�@@1'@@  @@  @@  @@  @?��@?�@>��@>ȴ@>��@>{@=��@=p�@=?}@<�@<I�@;ƨ@;��@;�@;dZ@;33@:�@:~�@9��@9�@8�9@8�u@8�@8A�@8  @7�;@7��@7;d@7
=@6�+@6$�@5�@5@5�h@5?}@5�@5V@4��@4�/@4I�@3ƨ@3��@3�@3dZ@3"�@2��@2M�@2-@1��@1��@1��@1�7@1&�@0�`@0Q�@/�P@/\)@/;d@.�@.��@.ff@.5?@-�@-��@-@-@-��@-p�@-/@,�/@,�D@,Z@,�@+ƨ@+��@+dZ@*�@*n�@*-@*�@)��@)��@)��@)�7@)hs@)&�@(��@(�@(1'@'�@'�P@';d@';d@'�@&��@&�y@&ȴ@&��@&{@%�h@%O�@%/@%�@$�/@$j@$I�@$(�@$�@#�m@#�F@#dZ@#33@"�@"�!@"��@"�\@"~�@"~�@"n�@"M�@!��@!hs@ ��@ Ĝ@ �9@ �u@ r�@�;@�P@K�@�@�@ȴ@ff@5?@@@�h@`B@�@�j@��@�D@I�@1@��@��@�@C�@"�@�@��@�\@=q@��@�@�#@��@��@x�@X@&�@��@�u@r�@bN@b@��@�P@l�@\)@;d@
=@�y@��@E�@$�@�T@@��@�@��@�@V@V@�@z�@z�@Z@1@�m@�
@��@dZ@S�@33@o@�!@�\@M�@�@�@�@��@��@x�@x�@X@X@G�@�@Ĝ@�@bN@�;@�@|�@K�@�@�y@ȴ@��@��@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�jA�z�A��A��#A�XA�$�A�ƨA�ĜA�ĜA߃A�VAܴ9A؍PA�^5A�O�A��
A�ZA�5?A�A�A˴9A�  A�jA��A�ȴAÏ\A¾wA�O�A���A�XA�r�A�XA�l�A��wA��-A�O�A�A�A��A�S�A�1'A��RA�K�A��A�t�A�S�A��/A�O�A�;dA�A�A��9A�z�A�7LA��!A��A�$�A�hsA��A���A���A�7LA��A���A�A�A�M�A�`BA��A��#A���A�(�A��A�%A��uA��A��FA�{A�A���A�&�A��`A���A��/A�%A��TA���A�ZA�=qA���A��TA�{A��^AA}
=A{�Ayx�Ax�`Ax~�AwG�At��ArZAop�Am|�Ak��Ai�Ai�Ag�^Af��Af��Ae`BAc�TAb��Aa��A_�-A^�uA]A\��A\{AY��AWXAU�hAU�PAU\)AR{AOAN~�AMƨALr�AH�yAF��ACC�A@~�A?+A>ffA=�7A;�^A:9XA9/A7�A6�9A5A2�jA2A0��A/VA-O�A,M�A+33A)x�A(��A(A&ȴA%��A%�A$�uA#�A#VA"�A"��A"�A!�
A �9A =qA�A33A��A�mA/A�#A;dAAĜA%Ax�AM�A1'A�A�A�A��AhsA�`A��A5?A�A��A(�A7LAffA^5AbA�TA`BA
^5A	%AE�AƨA��AVA��A+A��AG�A �R@�M�@�&�@�"�@�n�@�{@�=q@�~�@�$�@��^@���@�  @�@�p�@��@��@�v�@�?}@��;@��@�D@�K�@��H@�~�@�V@�ƨ@�ȴ@�5?@���@ݙ�@�p�@܋D@۶F@��@�J@�X@أ�@ו�@֟�@��@��@�9X@��
@��@�{@��@�1@�33@��@͉7@��`@� �@�~�@Ɂ@ȼj@�j@��@Ǯ@��@�J@�O�@�%@�j@�1@Õ�@�n�@�/@��u@��@���@��P@�l�@�@�M�@��@�@���@��@�G�@��/@�z�@��@�t�@���@���@�7L@���@�1@�  @�(�@���@�ȴ@�5?@���@�X@��@���@�r�@��w@�\)@��@��H@���@�^5@�E�@�-@���@��h@��@��@���@��@�b@��P@�o@��@�ȴ@�v�@��@���@���@�{@��@�O�@���@���@���@�\)@�C�@�@��y@���@�~�@��@��T@�M�@�5?@�M�@���@��@�V@�$�@�^5@��R@��H@��!@�M�@�$�@�M�@��@�@���@���@��h@�7L@�/@��`@��@���@���@��7@�`B@�&�@�`B@�`B@���@���@��#@�@�@��h@��@�p�@��9@��@�K�@�\)@�ȴ@��@���@��#@��-@���@��h@�`B@��@�Q�@��@��m@��F@��@���@�S�@��H@���@�n�@���@�-@���@�V@�z�@���@��w@��P@�~�@�$�@��#@��-@���@�7L@��@��@�j@�Q�@�Q�@���@�@�5?@�$�@���@��#@���@�x�@�p�@�O�@��@��/@���@�r�@�I�@�1'@��@���@��P@�;d@�"�@��y@���@��\@�E�@���@��-@�G�@��@�%@��@���@��@�r�@�1'@���@���@���@���@�|�@�S�@�+@��@�
=@��R@�@�`B@�%@��/@���@���@��u@�j@�(�@���@��w@�C�@���@��!@�~�@�=q@���@��h@��@�hs@�%@���@���@��@�I�@�b@�ƨ@�33@��H@��+@�M�@��#@���@�O�@��@�Ĝ@�j@��;@��@�dZ@�"�@�@�ȴ@�n�@�-@��@�@�hs@�?}@�bN@��
@��F@���@���@�33@��@��R@��+@�-@��#@���@�x�@�O�@�/@�&�@�&�@��@�V@��@�Ĝ@��@�1'@��@�1@�1@�@��@�P@\)@~��@~�R@~ff@~@|��@|�D@|j@|Z@|9X@{�m@{��@{�@{t�@{33@z�!@z=q@y��@y��@y�^@y�7@y&�@y�@x��@x�9@xbN@xb@w�;@w�P@wK�@w\)@w\)@wK�@w;d@w;d@v�y@v��@vv�@v$�@u@u�@uV@t�/@t�/@t�@tI�@s�m@s�@s"�@r��@r=q@q��@qx�@q&�@p�@o��@o\)@o\)@o+@n�@n�y@nv�@m�@m��@m�@m`B@m?}@mV@l�j@l�@k��@k�
@kƨ@k�F@ko@j��@jM�@i��@i&�@h��@g�;@gK�@f�@f��@fE�@e@eO�@d��@d��@d�j@d��@d�D@dz�@d9X@cS�@c@b��@b~�@b~�@bM�@b=q@b�@bJ@a��@ax�@`Q�@`  @_�@_�P@_
=@^�R@^v�@^@]��@]O�@]V@\��@\z�@\�@[ƨ@[t�@Z��@Y�@Yx�@YX@YG�@Y7L@Y&�@X�`@Xr�@W�;@W\)@V��@V�R@Vv�@VV@V$�@U�-@T��@Tj@S�
@S��@SC�@R�@R�!@R�\@R=q@Q��@Qhs@P�`@P�@P �@O�;@O�@O�@N�+@NE�@N{@M�@M@Mp�@MV@L��@LZ@L(�@Kƨ@KdZ@KC�@K@J�\@J=q@I��@I��@I�7@IG�@I%@HĜ@H�u@HbN@HA�@H �@G��@GK�@G+@Fv�@F5?@E�T@E�-@E/@D��@D�/@D�@Dz�@DI�@D(�@C�m@C�F@C��@Ct�@CC�@B�H@B�!@B��@B-@A�#@A�7@AG�@A�@@�9@@r�@@1'@@  @@  @@  @@  @?��@?�@>��@>ȴ@>��@>{@=��@=p�@=?}@<�@<I�@;ƨ@;��@;�@;dZ@;33@:�@:~�@9��@9�@8�9@8�u@8�@8A�@8  @7�;@7��@7;d@7
=@6�+@6$�@5�@5@5�h@5?}@5�@5V@4��@4�/@4I�@3ƨ@3��@3�@3dZ@3"�@2��@2M�@2-@1��@1��@1��@1�7@1&�@0�`@0Q�@/�P@/\)@/;d@.�@.��@.ff@.5?@-�@-��@-@-@-��@-p�@-/@,�/@,�D@,Z@,�@+ƨ@+��@+dZ@*�@*n�@*-@*�@)��@)��@)��@)�7@)hs@)&�@(��@(�@(1'@'�@'�P@';d@';d@'�@&��@&�y@&ȴ@&��@&{@%�h@%O�@%/@%�@$�/@$j@$I�@$(�@$�@#�m@#�F@#dZ@#33@"�@"�!@"��@"�\@"~�@"~�@"n�@"M�@!��@!hs@ ��@ Ĝ@ �9@ �u@ r�@�;@�P@K�@�@�@ȴ@ff@5?@@@�h@`B@�@�j@��@�D@I�@1@��@��@�@C�@"�@�@��@�\@=q@��@�@�#@��@��@x�@X@&�@��@�u@r�@bN@b@��@�P@l�@\)@;d@
=@�y@��@E�@$�@�T@@��@�@��@�@V@V@�@z�@z�@Z@1@�m@�
@��@dZ@S�@33@o@�!@�\@M�@�@�@�@��@��@x�@x�@X@X@G�@�@Ĝ@�@bN@�;@�@|�@K�@�@�y@ȴ@��@��@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�B
�B
�ZB
��B/Bq�BǮBhB�B�B&�B+B'�B�B��B�B�NB��B�
B�5B�HB�NB�mB�B�NB�B%BJBuB�B�B�B�B"�B"�B"�B�B�B�B�B�B!�B�B�B#�B(�B)�B,B7LB@�BB�B@�B9XB)�B�B+B��B�B�yB�ZB�5B�5B�B��B��B��BɺBȴB�}B�?B�3B��By�BdZBO�BE�B9XB/B#�B �BuB
��B
��B
�B
�TB
��B
��B
�wB
��B
� B
z�B
o�B
`BB
ZB
M�B
H�B
D�B
=qB
+B
�B
B	��B	�B	�NB	�/B	�
B	��B	��B	ȴB	�wB	�XB	�'B	��B	��B	��B	�hB	�\B	�B	w�B	iyB	hsB	e`B	[#B	J�B	C�B	=qB	7LB	'�B	�B	\B	B��B��B�B�B�`B�TB�;B�#B��B��B��BȴBÖB�qB�^B�RB�3B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�DB�B�B�B�1B�=B�PB�oB��B��B�-B�3B�?B�LB�FB�'B�9B�XB�dB�jB�XB�LB�?B�?B�FB�LB�3B�RB�RB�FB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�PB�\B�\B�bB�uB�uB�uB�uB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�LB�RB�^B�qBÖBǮBɺB��B��B��B��B��B�B�B�)B�/B�;B�ZB�B�B�B��B��B��B��B��B	B	%B		7B	PB	hB	oB	uB	�B	�B	�B	!�B	%�B	(�B	0!B	2-B	9XB	;dB	>wB	?}B	@�B	B�B	F�B	H�B	H�B	H�B	M�B	R�B	VB	ZB	]/B	_;B	`BB	cTB	ffB	gmB	k�B	o�B	p�B	o�B	o�B	r�B	v�B	x�B	y�B	y�B	}�B	�B	�%B	�7B	�1B	�7B	�DB	�PB	�\B	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�?B	�?B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	��B	�wB	�qB	�dB	�^B	�jB	�jB	��B	B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�)B	�)B	�BB	�HB	�;B	�;B	�;B	�;B	�;B	�;B	�HB	�HB	�BB	�BB	�NB	�TB	�ZB	�`B	�mB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
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
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
\B
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
k�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�B
�B
�ZB
��B/Bq�BǮBhB�B�B&�B+B'�B�B��B�B�NB��B�
B�5B�HB�NB�mB�B�NB�B%BJBuB�B�B�B�B"�B"�B"�B�B�B�B�B�B!�B�B�B#�B(�B)�B,B7LB@�BB�B@�B9XB)�B�B+B��B�B�yB�ZB�5B�5B�B��B��B��BɺBȴB�}B�?B�3B��By�BdZBO�BE�B9XB/B#�B �BuB
��B
��B
�B
�TB
��B
��B
�wB
��B
� B
z�B
o�B
`BB
ZB
M�B
H�B
D�B
=qB
+B
�B
B	��B	�B	�NB	�/B	�
B	��B	��B	ȴB	�wB	�XB	�'B	��B	��B	��B	�hB	�\B	�B	w�B	iyB	hsB	e`B	[#B	J�B	C�B	=qB	7LB	'�B	�B	\B	B��B��B�B�B�`B�TB�;B�#B��B��B��BȴBÖB�qB�^B�RB�3B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�DB�B�B�B�1B�=B�PB�oB��B��B�-B�3B�?B�LB�FB�'B�9B�XB�dB�jB�XB�LB�?B�?B�FB�LB�3B�RB�RB�FB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�PB�\B�\B�bB�uB�uB�uB�uB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�LB�RB�^B�qBÖBǮBɺB��B��B��B��B��B�B�B�)B�/B�;B�ZB�B�B�B��B��B��B��B��B	B	%B		7B	PB	hB	oB	uB	�B	�B	�B	!�B	%�B	(�B	0!B	2-B	9XB	;dB	>wB	?}B	@�B	B�B	F�B	H�B	H�B	H�B	M�B	R�B	VB	ZB	]/B	_;B	`BB	cTB	ffB	gmB	k�B	o�B	p�B	o�B	o�B	r�B	v�B	x�B	y�B	y�B	}�B	�B	�%B	�7B	�1B	�7B	�DB	�PB	�\B	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�?B	�?B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	��B	�wB	�qB	�dB	�^B	�jB	�jB	��B	B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�)B	�)B	�BB	�HB	�;B	�;B	�;B	�;B	�;B	�;B	�HB	�HB	�BB	�BB	�NB	�TB	�ZB	�`B	�mB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
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
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
\B
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
k�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20201012093845  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201012003910  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201012003911  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201012003912  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201012003912  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201012003912  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201012003913  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201012003913  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201012003913  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201012003913                      G�O�G�O�G�O�                JA  ARUP                                                                        20201012005457                      G�O�G�O�G�O�                