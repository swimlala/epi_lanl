CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-15T21:35:20Z creation;2018-08-15T21:35:23Z conversion to V3.1;2019-12-23T06:16:51Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܘ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20180815213520  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               RA   JA  I2_0675_082                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�y�.E�1   @�y��9 @8J	� ��c/ᰉ�'1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CDnCFnCH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D�RD!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�D׍�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��)D��D�P�D���D��)D�)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͡�A͡�Aͩ�Aͩ�AͬAͩ�Aͧ�Aͧ�Aͥ�A͕�A͍PA�dZA�9XA�{A��A��A���A̸RA�v�A��;A���A�bA�%A��
AüjA¡�A���A���A�%A�l�A��jA��A��7A��#A�jA�VA�dZA���A��mA�bNA���A���A���A��+A��A��DA�+A�A��7A�z�A�-A��PA���A���A�33A��A��uA��A�33A���A�1'A��A�z�A�"�A���A�XA���A�bNA���A��\A�{A���A�A��A�JA�ĜA��A��A�VA��jA��A��A�~�A�E�A��DA��A�-A�Q�A�K�A��A��DA��A��\A�
=A���A��A�&�A�A�5?A�ĜA�S�A�ffA�G�A�jA��A�dZA��+A�ȴA��TA�?}A�ƨA��`A��A�-A�oA�v�A�ffA��jA�#A~�!A}XA{�Ay|�Av�`As�
Aq�^An�`Al�Ak��AjjAidZAg�7AeO�Ab��A^�HA]&�A\JA[`BAX�AU�AS�wAR�\ARM�AQ�hAP��AO�AM��AJjAI%AGx�AE��ADn�ACXAB  A@��A?|�A>1'A<�`A;\)A9�A7�A7oA6�uA5�A4�+A2�yA1�PA-��A,�A+��A*��A(��A(1A&�A$VA#�;A#33A"1'A!��A!�A ��A �DA�FA��A-A��A��AE�A  A�A�uAJAG�A�/AI�A��A��AXA�AJAz�A1'A�A%AbNA�AM�AA%A{A
ȴA	p�A�;A7LA�!A��AAZA��A�A�DAAA��AXA ��A z�A I�@�ff@�?}@�bN@��y@��`@�z�@�
=@�V@���@��@��;@�@�$�@�hs@��
@�@�hs@�@���@�+@��@���@���@���@�z�@�ƨ@�
=@ޗ�@ݺ^@�b@���@�Z@ְ!@թ�@Դ9@�(�@��
@�+@���@��#@�%@���@�^5@�Ĝ@˾w@�;d@�^5@��#@��@�1@�l�@�+@Ɵ�@�p�@�Z@Å@�\)@� �@��m@Å@�G�@�@�J@�?}@��@�M�@�5?@��@��@�%@���@�  @��@���@�@��`@��F@�+@�@��@��@��y@�;d@�\)@�@��H@�n�@��#@��9@�33@��@���@�p�@���@��-@�7L@��@���@�A�@�dZ@�C�@�o@��H@��H@�5?@�`B@��j@��u@�j@�Q�@�1'@�b@�ƨ@��@��y@�@��@��9@��u@���@��@�ȴ@�M�@���@�x�@�O�@�V@��u@�bN@�I�@�1'@�1@�  @��@��P@�C�@�o@�@��H@���@��@��@��@��u@���@�K�@��H@��+@�V@�@���@��@��D@��@��m@���@�dZ@�33@�ȴ@��R@��!@���@���@�M�@��T@���@��-@���@�G�@���@��@�1'@�b@��m@��w@���@�K�@���@��H@�ȴ@���@��+@�V@�5?@��@���@��h@�O�@��@��`@��j@���@���@�j@�1'@��;@�ƨ@��w@���@�|�@�o@��@���@��!@���@�v�@�=q@��@��T@���@�/@��@�Ĝ@��D@��@�z�@�Q�@��@��@��m@��F@��@�"�@�o@��H@���@��y@��y@�
=@���@��m@��w@�l�@�S�@�C�@�;d@��y@��@���@���@�~�@�M�@�V@�^5@�ff@�v�@�M�@�^5@�V@�E�@��@���@�G�@�/@�/@��`@��j@���@��u@�r�@�Q�@�9X@��@���@���@�t�@�l�@�\)@��@�ȴ@�M�@�E�@�E�@�=q@�@���@���@�?}@���@�1'@� �@�1@���@��@�|�@�+@��@���@�ff@�-@�{@�@���@���@��@�Q�@�b@��@�@K�@~��@~E�@~$�@~@}�@|�@|9X@{�m@{��@{dZ@z�@z�\@z�@x�`@xA�@w�w@wl�@v�@vff@v@u��@u?}@uV@t��@t�j@tz�@t9X@t�@s��@s�F@st�@sS�@s33@r��@rM�@q�^@qX@q7L@p�`@p��@p1'@o|�@n�y@n��@n@m�-@l��@l�D@lI�@kƨ@k33@k"�@j�@j��@j=q@i�#@i�^@i�7@i7L@h��@hbN@hA�@g�w@gl�@fȴ@fv�@fff@f5?@e��@e�h@e�@d�@dz�@d�D@dj@c�
@cC�@c@b��@a��@aX@`Ĝ@`�u@`bN@`  @_l�@_;d@^��@^�+@^$�@]��@]�@]?}@]�@\��@\��@\�@\j@[�
@[dZ@[dZ@[C�@[33@[@Z�@Y��@Y��@XĜ@XbN@X  @W��@W\)@W�@V��@V5?@V$�@U��@U/@T�/@T�@T��@TZ@T�@S�
@R�H@Rn�@R�@Q�@Q��@Q�^@Q&�@P�u@P �@O�;@O��@OK�@O
=@N��@NE�@N5?@N@M��@M@M?}@L��@L�j@L��@Lz�@L1@K�
@K��@KS�@J��@J=q@I��@I�#@I��@I7L@H�9@Hr�@HQ�@HA�@H1'@G�@G�w@Gl�@G;d@F�y@Fv�@FE�@F@E�-@E�h@E�@E`B@E�@D�@D�@C�
@Cƨ@CS�@C"�@B�@B�\@BM�@A��@A�@A�#@A��@AX@A%@@�`@@��@@�u@@�@@A�@@  @?��@?�@?��@?�P@?|�@?;d@>ȴ@>��@>�R@>��@>$�@=�T@=@=�h@=�@<�/@<��@<Z@<I�@<I�@<I�@<(�@;t�@:�@:n�@:�@:J@9��@9�#@9�^@9��@9G�@8��@8�9@8�@8A�@7�@7�w@7K�@6��@6�y@6��@6$�@5��@5O�@5?}@4�/@4��@4�@3�
@3��@333@2�H@2�!@2~�@2-@1��@1�#@1��@1x�@1X@0��@0�u@0Q�@0 �@/�;@/�w@/�w@/��@/�P@/�P@/;d@.��@.E�@.@-��@-�-@-�@-O�@-/@,��@,�j@,�D@,9X@,�@+�m@+t�@*�@*��@*�\@*�\@*~�@*n�@*M�@)��@)�7@)x�@)X@)G�@)&�@(Ĝ@(��@(bN@(1'@(b@'�@'�w@'K�@'�@&�@&ȴ@&��@&v�@&E�@&{@%�@%@%?}@%V@$�/@$�@$I�@#�m@#��@#t�@#@"��@"��@!��@!hs@!7L@!7L@!7L@!7L@!&�@ ��@ ��@ �9@ r�@ 1'@�@��@�@�P@l�@K�@ȴ@�+@@�T@p�@�@�j@�j@�j@�@z�@j@(�@1@ƨ@ƨ@�F@�F@��@33@�H@�\@^5@^5@=q@��@��@��@X@&�@�`@Ĝ@�9@��@r�@Q�@A�@1'@b@��@|�@;d@�@��@�@��@E�@$�@@�@�T@��@��@p�@O�@�@�@��@��@9X@�m@�F@��@o@��@n�@M�@J@�#@��@�^@��@x�@7L@�@Ĝ@��@r�@bN@A�@�@�@�P@K�@K�@+@+@
=@
=@�y@��@�+@�+@v�@ff@{@�h@p�@`B@`B@`B@O�@?}@?}@?}@�@��@��@��@Z@9X@9X@(�@(�@�@�m@�@dZ@S�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͡�A͡�Aͩ�Aͩ�AͬAͩ�Aͧ�Aͧ�Aͥ�A͕�A͍PA�dZA�9XA�{A��A��A���A̸RA�v�A��;A���A�bA�%A��
AüjA¡�A���A���A�%A�l�A��jA��A��7A��#A�jA�VA�dZA���A��mA�bNA���A���A���A��+A��A��DA�+A�A��7A�z�A�-A��PA���A���A�33A��A��uA��A�33A���A�1'A��A�z�A�"�A���A�XA���A�bNA���A��\A�{A���A�A��A�JA�ĜA��A��A�VA��jA��A��A�~�A�E�A��DA��A�-A�Q�A�K�A��A��DA��A��\A�
=A���A��A�&�A�A�5?A�ĜA�S�A�ffA�G�A�jA��A�dZA��+A�ȴA��TA�?}A�ƨA��`A��A�-A�oA�v�A�ffA��jA�#A~�!A}XA{�Ay|�Av�`As�
Aq�^An�`Al�Ak��AjjAidZAg�7AeO�Ab��A^�HA]&�A\JA[`BAX�AU�AS�wAR�\ARM�AQ�hAP��AO�AM��AJjAI%AGx�AE��ADn�ACXAB  A@��A?|�A>1'A<�`A;\)A9�A7�A7oA6�uA5�A4�+A2�yA1�PA-��A,�A+��A*��A(��A(1A&�A$VA#�;A#33A"1'A!��A!�A ��A �DA�FA��A-A��A��AE�A  A�A�uAJAG�A�/AI�A��A��AXA�AJAz�A1'A�A%AbNA�AM�AA%A{A
ȴA	p�A�;A7LA�!A��AAZA��A�A�DAAA��AXA ��A z�A I�@�ff@�?}@�bN@��y@��`@�z�@�
=@�V@���@��@��;@�@�$�@�hs@��
@�@�hs@�@���@�+@��@���@���@���@�z�@�ƨ@�
=@ޗ�@ݺ^@�b@���@�Z@ְ!@թ�@Դ9@�(�@��
@�+@���@��#@�%@���@�^5@�Ĝ@˾w@�;d@�^5@��#@��@�1@�l�@�+@Ɵ�@�p�@�Z@Å@�\)@� �@��m@Å@�G�@�@�J@�?}@��@�M�@�5?@��@��@�%@���@�  @��@���@�@��`@��F@�+@�@��@��@��y@�;d@�\)@�@��H@�n�@��#@��9@�33@��@���@�p�@���@��-@�7L@��@���@�A�@�dZ@�C�@�o@��H@��H@�5?@�`B@��j@��u@�j@�Q�@�1'@�b@�ƨ@��@��y@�@��@��9@��u@���@��@�ȴ@�M�@���@�x�@�O�@�V@��u@�bN@�I�@�1'@�1@�  @��@��P@�C�@�o@�@��H@���@��@��@��@��u@���@�K�@��H@��+@�V@�@���@��@��D@��@��m@���@�dZ@�33@�ȴ@��R@��!@���@���@�M�@��T@���@��-@���@�G�@���@��@�1'@�b@��m@��w@���@�K�@���@��H@�ȴ@���@��+@�V@�5?@��@���@��h@�O�@��@��`@��j@���@���@�j@�1'@��;@�ƨ@��w@���@�|�@�o@��@���@��!@���@�v�@�=q@��@��T@���@�/@��@�Ĝ@��D@��@�z�@�Q�@��@��@��m@��F@��@�"�@�o@��H@���@��y@��y@�
=@���@��m@��w@�l�@�S�@�C�@�;d@��y@��@���@���@�~�@�M�@�V@�^5@�ff@�v�@�M�@�^5@�V@�E�@��@���@�G�@�/@�/@��`@��j@���@��u@�r�@�Q�@�9X@��@���@���@�t�@�l�@�\)@��@�ȴ@�M�@�E�@�E�@�=q@�@���@���@�?}@���@�1'@� �@�1@���@��@�|�@�+@��@���@�ff@�-@�{@�@���@���@��@�Q�@�b@��@�@K�@~��@~E�@~$�@~@}�@|�@|9X@{�m@{��@{dZ@z�@z�\@z�@x�`@xA�@w�w@wl�@v�@vff@v@u��@u?}@uV@t��@t�j@tz�@t9X@t�@s��@s�F@st�@sS�@s33@r��@rM�@q�^@qX@q7L@p�`@p��@p1'@o|�@n�y@n��@n@m�-@l��@l�D@lI�@kƨ@k33@k"�@j�@j��@j=q@i�#@i�^@i�7@i7L@h��@hbN@hA�@g�w@gl�@fȴ@fv�@fff@f5?@e��@e�h@e�@d�@dz�@d�D@dj@c�
@cC�@c@b��@a��@aX@`Ĝ@`�u@`bN@`  @_l�@_;d@^��@^�+@^$�@]��@]�@]?}@]�@\��@\��@\�@\j@[�
@[dZ@[dZ@[C�@[33@[@Z�@Y��@Y��@XĜ@XbN@X  @W��@W\)@W�@V��@V5?@V$�@U��@U/@T�/@T�@T��@TZ@T�@S�
@R�H@Rn�@R�@Q�@Q��@Q�^@Q&�@P�u@P �@O�;@O��@OK�@O
=@N��@NE�@N5?@N@M��@M@M?}@L��@L�j@L��@Lz�@L1@K�
@K��@KS�@J��@J=q@I��@I�#@I��@I7L@H�9@Hr�@HQ�@HA�@H1'@G�@G�w@Gl�@G;d@F�y@Fv�@FE�@F@E�-@E�h@E�@E`B@E�@D�@D�@C�
@Cƨ@CS�@C"�@B�@B�\@BM�@A��@A�@A�#@A��@AX@A%@@�`@@��@@�u@@�@@A�@@  @?��@?�@?��@?�P@?|�@?;d@>ȴ@>��@>�R@>��@>$�@=�T@=@=�h@=�@<�/@<��@<Z@<I�@<I�@<I�@<(�@;t�@:�@:n�@:�@:J@9��@9�#@9�^@9��@9G�@8��@8�9@8�@8A�@7�@7�w@7K�@6��@6�y@6��@6$�@5��@5O�@5?}@4�/@4��@4�@3�
@3��@333@2�H@2�!@2~�@2-@1��@1�#@1��@1x�@1X@0��@0�u@0Q�@0 �@/�;@/�w@/�w@/��@/�P@/�P@/;d@.��@.E�@.@-��@-�-@-�@-O�@-/@,��@,�j@,�D@,9X@,�@+�m@+t�@*�@*��@*�\@*�\@*~�@*n�@*M�@)��@)�7@)x�@)X@)G�@)&�@(Ĝ@(��@(bN@(1'@(b@'�@'�w@'K�@'�@&�@&ȴ@&��@&v�@&E�@&{@%�@%@%?}@%V@$�/@$�@$I�@#�m@#��@#t�@#@"��@"��@!��@!hs@!7L@!7L@!7L@!7L@!&�@ ��@ ��@ �9@ r�@ 1'@�@��@�@�P@l�@K�@ȴ@�+@@�T@p�@�@�j@�j@�j@�@z�@j@(�@1@ƨ@ƨ@�F@�F@��@33@�H@�\@^5@^5@=q@��@��@��@X@&�@�`@Ĝ@�9@��@r�@Q�@A�@1'@b@��@|�@;d@�@��@�@��@E�@$�@@�@�T@��@��@p�@O�@�@�@��@��@9X@�m@�F@��@o@��@n�@M�@J@�#@��@�^@��@x�@7L@�@Ĝ@��@r�@bN@A�@�@�@�P@K�@K�@+@+@
=@
=@�y@��@�+@�+@v�@ff@{@�h@p�@`B@`B@`B@O�@?}@?}@?}@�@��@��@��@Z@9X@9X@(�@(�@�@�m@�@dZ@S�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�%B�1B�=B�PB�hB��B��B��B��B��B��B��B�FBŢBŢB��B��B�5B�TB�yB�B�B��B�B�B�BoB�B8RBC�BL�BR�BVBYB`BBaHBdZBffBgmBo�Bo�Bt�Bu�Bu�Bv�Bu�Bw�Bv�Bx�Bx�Bw�Bv�Bu�Bw�B{�B� B�1B�DB��B��B��B�B�B�RB��B��B��BŢB��BƨB�RB��B�{B�\B� Be`BT�B<jB+B�B
=BB�B�HB�NB�5B��B�FB��B��B�VB|�BgmBXBN�BB�B49B#�B�B
��B
�B
ǮB
�?B
��B
�bB
m�B
H�B
oB	��B	��B	�B	�ZB	��B	�dB	��B	�\B	z�B	t�B	o�B	gmB	[#B	O�B	<jB	0!B	�B	\B	%B	B��B�NB�B��B��B��BĜB��B�3B�{B�1B�Bw�Bs�Bp�Bl�BffB^5BZB\)B\)BW
BQ�BYBW
B]/BW
BO�BH�BB�B7LB5?B49B/B1'B0!B.B.B/B2-B1'B33B33B33B5?B33B1'B1'B1'B0!B0!B1'B0!B.B.B,B,B,B+B+B+B)�B%�B%�B$�B%�B+B+B+B)�B'�B'�B%�B$�B"�B!�B �B �B�B�B�B�B�B!�B!�B!�B!�B!�B!�B �B$�B#�B$�B%�B#�B$�B&�B%�B%�B%�B&�B&�B%�B$�B$�B#�B$�B$�B#�B#�B#�B"�B#�B&�B&�B'�B)�B(�B&�B$�B!�B#�B%�B$�B#�B#�B#�B$�B%�B'�B'�B(�B,B.B/B0!B1'B1'B33B5?B5?B5?B6FB9XB;dB<jB>wBL�BL�BL�BL�BL�BN�BP�BQ�BS�BT�BT�BVBXB\)B_;B_;BdZBjBiyBl�Bo�Bp�Br�Bw�By�B{�B}�B~�B� B�B�%B�DB�hB�{B��B��B��B��B��B��B��B��B�B�B�-B�?B�FB�^B�wBÖBÖBĜBĜBŢBƨBȴB��B��B��B��B��B��B��B��B��B�B�)B�/B�BB�fB�B�B�B��B��B��B��B��B��B	B	B	B	B	DB	VB	VB	oB	hB	oB	{B	�B	�B	�B	�B	"�B	&�B	-B	/B	2-B	49B	6FB	<jB	>wB	>wB	>wB	>wB	A�B	E�B	F�B	G�B	H�B	L�B	O�B	P�B	W
B	YB	[#B	\)B	]/B	`BB	cTB	dZB	e`B	gmB	iyB	l�B	n�B	o�B	t�B	w�B	y�B	|�B	~�B	� B	�B	�B	�B	�B	�1B	�JB	�JB	�PB	�VB	�bB	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�3B	�3B	�3B	�9B	�FB	�^B	�^B	�jB	B	ƨB	ǮB	ǮB	ǮB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�;B	�5B	�;B	�HB	�HB	�HB	�NB	�TB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
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
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
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
W
B
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
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
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
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
hsB
hsB
hsB
iyB
iyB
iyB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�B��B��B��B�B��B��B�B�	B�B�4B�YB�YB�xB��B��B��B��B�B�mB�mB��BҽB�B�:B�DB�WB�KB��BqB�BqB:BqB8BCaBL�BR�BU�BX�B`BaBd&Bf2Bg8BoiBoiBt�Bu�Bu�Bv�Bu�Bw�Bv�Bx�Bx�Bw�Bv�Bu�Bw�B{�B�B��B�B�SB��B��B��B��B�B͟BΥB̘B�mBϫB�tB�B��B�FB�(B�Be,BT�B<6B*�BqB
	B�B�vB�B�B�B͟B�B��B�_B�"B|�Bg8BW�BN�BB[B4B#�BSB
��B
��B
�zB
�B
��B
�.B
m]B
H�B
:B	��B	�B	�]B	�&B	��B	�0B	��B	�(B	z�B	t�B	oiB	g8B	Z�B	O�B	<6B	/�B	jB	(B	�B	 �B�B�B��BϫB͟BʌB�gB�OB��B�,B��B��Bw�Bs�BpoBlWBf2B^BY�B[�B[�BV�BQ�BX�BV�B\�BV�BO�BH�BB[B7B5B4B.�B0�B/�B-�B-�B.�B1�B0�B2�B2�B2�B5B2�B0�B0�B0�B/�B/�B0�B/�B-�B-�B+�B+�B+�B*�B*�B*�B)�B%�B%�B$�B%�B*�B*�B*�B)�B'�B'�B%�B$�B"�B!|B �B vBpB�BjB�B�B!|B!�B!|B!�B!�B!�B �B$�B#�B$�B%�B#�B$�B&�B%�B%�B%�B&�B&�B%�B$�B$�B#�B$�B$�B#�B#�B#�B"�B#�B&�B&�B'�B)�B(�B&�B$�B!|B#�B%�B$�B#�B#�B#�B$�B%�B'�B'�B(�B+�B-�B.�B/�B0�B0�B2�B5B5B5B5�B9	B;B<6B>(BL�BL�BL~BL�BL~BN�BP�BQ�BS�BT�BT�BU�BW�B[�B^�B_Bd&Bj0BiDBl=BoOBpUBraBw�By�B{�B}�B~�B�B��B��B�B�4B�,B�SB�?B�dB��B��B��B��B��B��B��B��B��B�B�*B�(B�aB�GB�MB�MB�SB�tB�fB�rBʌB�~BΊBϑBϑBЗBѷBңB��B��B��B�B�2B�cB�UB�[B�nB�zB��B��B��B��B	 �B	�B	�B	�B	
�B	"B	B	:B	4B	 B	FB	EB	qB	qB	]B	"�B	&�B	,�B	.�B	1�B	3�B	5�B	<B	>(B	>(B	>(B	>BB	AUB	ESB	FYB	GzB	HfB	L~B	O�B	P�B	V�B	X�B	Z�B	[�B	\�B	_�B	c B	dB	eB	gB	iDB	l=B	nIB	oOB	tnB	w�B	y�B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�B	�4B	�4B	� B	�2B	�kB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�AB	�tB	�_B	�_B	�_B	�tB	�YB	�YB	�zB	�_B	�_B	�zB	�_B	�zB	ȀB	�fB	ʌB	ʌB	�~B	̈́B	ΥB	ЗB	ѝB	ѝB	ңB	ңB	ԯB	��B	��B	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�$B	�*B	�*B	�QB	�6B	�WB	�=B	�WB	�CB	�CB	�]B	�cB	�cB	�OB	�UB	�vB	�|B	�hB	�B	�B	�tB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
B
B

�B
�B
�B
B
B
B
B
B
B
"B
B
(B
.B
B
4B
B
B
 B
 B
 B
&B
&B
@B
FB
,B
,B
,B
FB
,B
,B
2B
FB
FB
,B
MB
MB
2B
MB
2B
9B
SB
?B
?B
YB
YB
YB
EB
eB
EB
KB
QB
WB
WB
xB
WB
qB
]B
xB
~B
�B
jB
jB
pB
pB
�B
pB
 vB
!�B
!�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
*�B
*�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4B
3�B
3�B
5B
4�B
4�B
5�B
5�B
6�B
7B
7B
7B
8B
8B
8B
8B
9$B
8B
8B
9$B
:B
;0B
;B
;0B
;B
<B
<B
<6B
=<B
="B
="B
>BB
>BB
>(B
>(B
?HB
?.B
?.B
?HB
@4B
AUB
A;B
AUB
A;B
AUB
AUB
A;B
A;B
BAB
B[B
B[B
CGB
CaB
CaB
CGB
DgB
DgB
DMB
EmB
ESB
ESB
ESB
EmB
FtB
FtB
FtB
FYB
FYB
FtB
GzB
H�B
H�B
IlB
I�B
I�B
I�B
I�B
J�B
JrB
JrB
J�B
JrB
JrB
KxB
K�B
K�B
K�B
L~B
L�B
L~B
L~B
L~B
L~B
M�B
L�B
L~B
L�B
M�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
_B
^�B
_B
^�B
^�B
^�B
^�B
_B
^�B
_B
_�B
`B
_�B
_�B
`B
`�B
aB
`�B
a�B
a�B
bB
a�B
a�B
a�B
bB
bB
bB
bB
c B
cB
c B
c B
d&B
dB
e,B
eB
e,B
eB
eB
eB
e,B
f2B
fB
fB
fB
g8B
gB
gB
g8B
gB
gB
gB
h$B
h$B
h$B
h$B
h$B
h>B
h>B
h$B
i*B
i*B
i*B
iDB
i*B
iDB
j0B
jKB
j0B
j0B
j0B
jKB
j0B
j0B
j0B
jKB
j0B
j0B
k6B
kQB
k6B
k6B
kQB
kQB
k6B
kQB
lWB
l=B
l=B
l=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808210042142018082100421420180821004214201808220039302018082200393020180822003930JA  ARFMdecpA19c                                                                20180816063518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180815213520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180815213521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180815213521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180815213522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180815213522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180815213522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180815213522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180815213523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180815213523                      G�O�G�O�G�O�                JA  ARUP                                                                        20180815215529                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180816154313  CV  JULD            G�O�G�O�F��j                JM  ARCAJMQC2.0                                                                 20180820154214  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180820154214  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180821153930  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                