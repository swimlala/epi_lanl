CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-07T18:36:42Z creation;2019-04-07T18:36:45Z conversion to V3.1;2019-12-23T06:04:10Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190407183642  20200120031518  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_137                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ش��� 1   @ش��8�@8���rG�c3�rGE1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�<�D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @b�\@�z�@�z�A
=qA*=qAJ=qAj=qA��A��A��A��A��A��A��A��B�\B
�\B�\B�\B"�\B*�\B2�\B:�\BB�\BJ�\BR�\BZ�\Bb�\Bj�\Br�\Bz�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D	(�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'(�D'��D((�D(��D)(�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3/\D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<(�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD(�DD��DE(�DE��DF(�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR��DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX��DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db(�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk(�Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt��Du(�Du��Dv(�Dv��Dw(�Dw��Dx(�Dx��Dy(�Dy��Dz(�Dz��D{(�D{��D|(�D|��D}(�D}��D~(�D~��D(�D��D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��HD�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D�׮D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D{D��{D�{D�T{DÔ{D��{D�{D�T{DĔ{D��{D�{D�T{DŔ{D��{D�{D�T{DƔ{D��{D�{D�T{Dǔ{D��{D�{D�T{DȔ{D��{D�{D�T{Dɔ{D��{D�{D�T{Dʔ{D��{D�{D�T{D˔{D��{D�{D�T{D̔{D��{D�{D�T{D͔{D��{D�{D�T{DΔ{D��{D�{D�T{Dϔ{D��{D�{D�T{DД{D��{D�{D�T{Dє{D��{D�{D�T{DҔ{D��{D�{D�T{DӔ{D��{D�{D�QHDԑHD��{D�{D�T{DՔ{D��{D�{D�T{D֔{D��{D�{D�T{Dה{D��{D�{D�T{Dؔ{D��{D�{D�T{Dٔ{D��{D�{D�T{Dڔ{D��{D�{D�T{D۔{D��{D�{D�T{Dܔ{D��{D�{D�T{Dݔ{D��{D�{D�T{Dޔ{D��{D�{D�T{Dߔ{D��{D�{D�T{D���D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�HD��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D���D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�1A�bA�oA�{A�oA�{A�oA��A��A��A��A�VA�%A���A��;A��jA���A��PA�n�A�Q�A�{A���A��hA�z�A�7LA���A�r�A�p�A�;dA���A�1'A��A��HA�\)A�C�A�ĜA��A���A�;dA��A��7A�ĜA�ZA�9XA���A��jA�`BA��RA�`BA���A��A�-A���A���A�G�A��A�ZA�{A���A��A��PA�
=A�9XA��yA���A�9XA�$�A�r�A�ƨA��7A�ƨA�VA��-A�"�A�9XA�VA�bA�bNA���A�z�A��
A���A�p�A���A}O�Ay��Ax  Av�/AuAq�mAn��Akl�Ai��Af �Ac�;Ab��A_S�A]`BA\��A\jA\  A[��A[?}AZ��AZffAW"�AU\)ASx�AQ�7AN^5AL �AJ��AH�\AG�AG;dAGAF �AC��AA��A?�TA>9XA<��A:�DA9?}A8��A8A�A8  A7oA4bNA2ffA1|�A/�hA-�wA,�uA,  A+�wA*��A)�FA)�A(v�A(�A'��A'\)A'7LA'
=A&�A&��A&��A&JA%x�A%\)A#�A"bA!�^A 5?A�A�A�\A��AK�A5?AC�A�jAbNA��AC�AjAE�AG�AK�A  AS�A�A��A�jA��A��A
�yA
�A
bNA	��A	�hA	33A�Ap�A�AM�A�^A��At�Ar�A�hA �y@��@�j@�"�@��+@�@�O�@��j@�Q�@�ƨ@��!@��T@�7L@���@�-@�p�@�j@��@�|�@�o@���@�^5@�7@�o@���@�n�@�-@��@�5?@�j@�S�@�v�@�-@���@���@ݑh@�9X@ۍP@�$�@�Ĝ@���@�K�@��@�ȴ@�-@��@ՙ�@�?}@�33@Гu@�dZ@�{@�Q�@�v�@��T@ɺ^@���@���@�
=@�5?@��@Ų-@ŉ7@�p�@�7L@���@�1'@���@Ý�@�ȴ@���@��`@���@�"�@��@�r�@�ȴ@�=q@�E�@�^5@�@�X@�/@��/@�9X@�1@�ȴ@��^@���@�I�@�z�@��@��F@��@���@��@��@�K�@���@���@��T@��@���@��F@�t�@�K�@�
=@��H@���@�~�@�~�@�v�@��@�`B@�?}@�V@��@��@��@��@�A�@� �@�C�@�
=@��@�~�@���@�7L@��/@�Ĝ@��j@��9@��@�A�@�  @��m@��w@��F@��F@��w@��
@���@�dZ@�"�@��y@���@�ff@�E�@�J@��T@���@�?}@��u@��F@�@���@���@�~�@�v�@�=q@���@�hs@�&�@�O�@�X@�?}@��@��@�Z@�(�@��
@�;d@��@��H@���@���@���@��+@��+@�~�@�V@�@��#@�x�@�?}@���@��@�bN@� �@��F@���@��P@�C�@�+@��@��\@�E�@��#@��#@���@��-@�G�@��@��j@���@�bN@�b@�  @�  @��m@��@�t�@�+@���@���@��\@�n�@�J@��T@��-@�hs@���@��j@��@�Z@� �@���@��m@��w@��F@���@�t�@�"�@��H@��R@���@���@���@�-@�@�@�@�J@�J@��T@���@�X@�X@�%@��`@���@��@�bN@�(�@� �@��@��m@�ƨ@���@�;d@�
=@�@�@��@���@���@��R@���@���@��+@�~�@�~�@�~�@�~�@�v�@�V@�{@��@���@�@��-@��^@��-@���@�O�@��@�%@��j@�z�@�Z@� �@�1@�  @��;@��w@��P@�dZ@�C�@�"�@�"�@�"�@��@���@��@��+@��@���@�@���@��@�O�@�/@�Ĝ@���@���@���@�bN@�(�@�@K�@~ff@~$�@}�@}�-@}O�@|��@|�j@|�D@|�@{��@{S�@{"�@z��@z^5@yx�@x�@x1'@w�@w�@wl�@w+@vȴ@v@u�@uO�@t�@t�@t�D@tI�@sƨ@s��@st�@s33@s@rJ@q�@q��@qG�@q&�@p��@pA�@o�@o��@o�@nv�@n{@m�-@m/@l�@lz�@lI�@k��@k"�@j-@i�^@i�7@iX@i&�@i%@h��@h�9@hbN@h1'@h  @g�P@g\)@g
=@f�+@f$�@e��@e�-@e�@ep�@d��@dI�@d1@c�
@c��@c�@cS�@co@b��@bJ@a�7@`�`@`Q�@`b@_�;@_��@_l�@_;d@_+@_�@_
=@^�y@^�+@]�@]�@\��@\��@\��@\Z@[�m@[��@[�@[dZ@[33@Z~�@Z=q@Y�#@YX@X�`@X��@XbN@Xb@W��@W�P@W\)@WK�@W;d@W�@V��@V�+@Vff@V$�@U��@U�@UV@T�@T�j@T�@T�D@Tj@T9X@T1@S�F@S�F@S��@St�@S33@R�@R��@R~�@Rn�@Rn�@Rn�@RM�@R�@Q�#@Q��@Q�^@Qx�@QX@Q7L@P��@P�u@Pr�@PA�@P1'@P �@O�@O
=@N�R@N��@N�+@NV@N$�@M�@M/@L�@L�/@L�j@L�@L��@LI�@L1@K�
@K��@K��@K��@KdZ@Ko@J��@J�!@J�\@J^5@JM�@J-@I�@Ihs@I�@I%@H�`@HĜ@HQ�@H  @G�;@G|�@G\)@G�@G
=@F�R@FE�@F@E��@Ep�@E/@D�j@D9X@C�
@C��@CS�@Co@Bn�@BJ@A��@A�#@A�^@A�7@A7L@A%@@��@@bN@?l�@>��@>ff@>E�@>@=@=p�@<�/@<��@<9X@;�
@;��@;t�@;o@:��@:-@9�@8��@8�9@8bN@7�@7�@7K�@6ȴ@6�+@6$�@5��@5�@4��@4��@4�j@4�j@4�@4�@4z�@4j@49X@3�m@3��@3C�@333@3o@3@2��@2n�@2�@1�@1��@1G�@0�`@0A�@0A�@0 �@/�@/��@/l�@/+@.��@.��@.�+@.5?@-`B@-?}@-�@,��@,�j@,��@,�D@,z�@,1@+�
@+��@+o@*�H@*��@*~�@*^5@*M�@*=q@*-@*�@*J@)�^@)x�@)X@)%@(��@(�@(bN@(A�@(  @'��@'��@'�P@';d@'�@'
=@'
=@&��@&�@&�@&��@&�+@&v�@&ff@&V@&{@%��@%@%��@%�h@%�@%`B@%O�@%?}@%V@$��@$j@$9X@$1@#�m@#ƨ@#��@#t�@#dZ@#dZ@#dZ@#S�@#C�@#33@#o@"��@"��@"n�@"�@!��@!�@!��@!��@!�^@!�^@!x�@!7L@!�@ Ĝ@ r�@ Q�@ A�@   @��@l�@�@�@v�@v�@V@5?@$�@@�@�h@�j@j@9X@(�@(�@1@��@�
@��@�@t�@t�@dZ@o@@@n�@-@�#@�^@x�@&�@Ĝ@ �@��@��@|�@\)@K�@+@�@
=@��@�@v�@$�@@`B@�@��@9X@�
@�F@�@dZ@~�@�@�^@��@�7@�7@�7@x�@hs@G�@X@X@G�@7L@7L@��@bN@ �@b@�;@�@|�@+@
=@�@v�@5?@$�@{@@�@�@�@��@@@�-@p�@O�@V@��@�@�@��@�j@�D@Z@��@��@S�@
�H@
^5@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�1A�bA�oA�{A�oA�{A�oA��A��A��A��A�VA�%A���A��;A��jA���A��PA�n�A�Q�A�{A���A��hA�z�A�7LA���A�r�A�p�A�;dA���A�1'A��A��HA�\)A�C�A�ĜA��A���A�;dA��A��7A�ĜA�ZA�9XA���A��jA�`BA��RA�`BA���A��A�-A���A���A�G�A��A�ZA�{A���A��A��PA�
=A�9XA��yA���A�9XA�$�A�r�A�ƨA��7A�ƨA�VA��-A�"�A�9XA�VA�bA�bNA���A�z�A��
A���A�p�A���A}O�Ay��Ax  Av�/AuAq�mAn��Akl�Ai��Af �Ac�;Ab��A_S�A]`BA\��A\jA\  A[��A[?}AZ��AZffAW"�AU\)ASx�AQ�7AN^5AL �AJ��AH�\AG�AG;dAGAF �AC��AA��A?�TA>9XA<��A:�DA9?}A8��A8A�A8  A7oA4bNA2ffA1|�A/�hA-�wA,�uA,  A+�wA*��A)�FA)�A(v�A(�A'��A'\)A'7LA'
=A&�A&��A&��A&JA%x�A%\)A#�A"bA!�^A 5?A�A�A�\A��AK�A5?AC�A�jAbNA��AC�AjAE�AG�AK�A  AS�A�A��A�jA��A��A
�yA
�A
bNA	��A	�hA	33A�Ap�A�AM�A�^A��At�Ar�A�hA �y@��@�j@�"�@��+@�@�O�@��j@�Q�@�ƨ@��!@��T@�7L@���@�-@�p�@�j@��@�|�@�o@���@�^5@�7@�o@���@�n�@�-@��@�5?@�j@�S�@�v�@�-@���@���@ݑh@�9X@ۍP@�$�@�Ĝ@���@�K�@��@�ȴ@�-@��@ՙ�@�?}@�33@Гu@�dZ@�{@�Q�@�v�@��T@ɺ^@���@���@�
=@�5?@��@Ų-@ŉ7@�p�@�7L@���@�1'@���@Ý�@�ȴ@���@��`@���@�"�@��@�r�@�ȴ@�=q@�E�@�^5@�@�X@�/@��/@�9X@�1@�ȴ@��^@���@�I�@�z�@��@��F@��@���@��@��@�K�@���@���@��T@��@���@��F@�t�@�K�@�
=@��H@���@�~�@�~�@�v�@��@�`B@�?}@�V@��@��@��@��@�A�@� �@�C�@�
=@��@�~�@���@�7L@��/@�Ĝ@��j@��9@��@�A�@�  @��m@��w@��F@��F@��w@��
@���@�dZ@�"�@��y@���@�ff@�E�@�J@��T@���@�?}@��u@��F@�@���@���@�~�@�v�@�=q@���@�hs@�&�@�O�@�X@�?}@��@��@�Z@�(�@��
@�;d@��@��H@���@���@���@��+@��+@�~�@�V@�@��#@�x�@�?}@���@��@�bN@� �@��F@���@��P@�C�@�+@��@��\@�E�@��#@��#@���@��-@�G�@��@��j@���@�bN@�b@�  @�  @��m@��@�t�@�+@���@���@��\@�n�@�J@��T@��-@�hs@���@��j@��@�Z@� �@���@��m@��w@��F@���@�t�@�"�@��H@��R@���@���@���@�-@�@�@�@�J@�J@��T@���@�X@�X@�%@��`@���@��@�bN@�(�@� �@��@��m@�ƨ@���@�;d@�
=@�@�@��@���@���@��R@���@���@��+@�~�@�~�@�~�@�~�@�v�@�V@�{@��@���@�@��-@��^@��-@���@�O�@��@�%@��j@�z�@�Z@� �@�1@�  @��;@��w@��P@�dZ@�C�@�"�@�"�@�"�@��@���@��@��+@��@���@�@���@��@�O�@�/@�Ĝ@���@���@���@�bN@�(�@�@K�@~ff@~$�@}�@}�-@}O�@|��@|�j@|�D@|�@{��@{S�@{"�@z��@z^5@yx�@x�@x1'@w�@w�@wl�@w+@vȴ@v@u�@uO�@t�@t�@t�D@tI�@sƨ@s��@st�@s33@s@rJ@q�@q��@qG�@q&�@p��@pA�@o�@o��@o�@nv�@n{@m�-@m/@l�@lz�@lI�@k��@k"�@j-@i�^@i�7@iX@i&�@i%@h��@h�9@hbN@h1'@h  @g�P@g\)@g
=@f�+@f$�@e��@e�-@e�@ep�@d��@dI�@d1@c�
@c��@c�@cS�@co@b��@bJ@a�7@`�`@`Q�@`b@_�;@_��@_l�@_;d@_+@_�@_
=@^�y@^�+@]�@]�@\��@\��@\��@\Z@[�m@[��@[�@[dZ@[33@Z~�@Z=q@Y�#@YX@X�`@X��@XbN@Xb@W��@W�P@W\)@WK�@W;d@W�@V��@V�+@Vff@V$�@U��@U�@UV@T�@T�j@T�@T�D@Tj@T9X@T1@S�F@S�F@S��@St�@S33@R�@R��@R~�@Rn�@Rn�@Rn�@RM�@R�@Q�#@Q��@Q�^@Qx�@QX@Q7L@P��@P�u@Pr�@PA�@P1'@P �@O�@O
=@N�R@N��@N�+@NV@N$�@M�@M/@L�@L�/@L�j@L�@L��@LI�@L1@K�
@K��@K��@K��@KdZ@Ko@J��@J�!@J�\@J^5@JM�@J-@I�@Ihs@I�@I%@H�`@HĜ@HQ�@H  @G�;@G|�@G\)@G�@G
=@F�R@FE�@F@E��@Ep�@E/@D�j@D9X@C�
@C��@CS�@Co@Bn�@BJ@A��@A�#@A�^@A�7@A7L@A%@@��@@bN@?l�@>��@>ff@>E�@>@=@=p�@<�/@<��@<9X@;�
@;��@;t�@;o@:��@:-@9�@8��@8�9@8bN@7�@7�@7K�@6ȴ@6�+@6$�@5��@5�@4��@4��@4�j@4�j@4�@4�@4z�@4j@49X@3�m@3��@3C�@333@3o@3@2��@2n�@2�@1�@1��@1G�@0�`@0A�@0A�@0 �@/�@/��@/l�@/+@.��@.��@.�+@.5?@-`B@-?}@-�@,��@,�j@,��@,�D@,z�@,1@+�
@+��@+o@*�H@*��@*~�@*^5@*M�@*=q@*-@*�@*J@)�^@)x�@)X@)%@(��@(�@(bN@(A�@(  @'��@'��@'�P@';d@'�@'
=@'
=@&��@&�@&�@&��@&�+@&v�@&ff@&V@&{@%��@%@%��@%�h@%�@%`B@%O�@%?}@%V@$��@$j@$9X@$1@#�m@#ƨ@#��@#t�@#dZ@#dZ@#dZ@#S�@#C�@#33@#o@"��@"��@"n�@"�@!��@!�@!��@!��@!�^@!�^@!x�@!7L@!�@ Ĝ@ r�@ Q�@ A�@   @��@l�@�@�@v�@v�@V@5?@$�@@�@�h@�j@j@9X@(�@(�@1@��@�
@��@�@t�@t�@dZ@o@@@n�@-@�#@�^@x�@&�@Ĝ@ �@��@��@|�@\)@K�@+@�@
=@��@�@v�@$�@@`B@�@��@9X@�
@�F@�@dZ@~�@�@�^@��@�7@�7@�7@x�@hs@G�@X@X@G�@7L@7L@��@bN@ �@b@�;@�@|�@+@
=@�@v�@5?@$�@{@@�@�@�@��@@@�-@p�@O�@V@��@�@�@��@�j@�D@Z@��@��@S�@
�H@
^5@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�-B
�-B
�3B
�3B
�?B
��B
ĜB
ƨB
��B�B#�B$�B1'BA�B\)Bt�B�VB�{B�BƨBǮBƨB��B��B��B�uB��B��B��B��B��B�PBo�BT�B@�B;dB7LB8RB5?B2-B-B(�B%�B�B�BbB+B
��B
��B
�B
�NB
�)B
�B  B
��B
�TB
�B
��B
ÖB
��B
�PB
� B
n�B
XB
B�B
%�B	��B	�sB	��B	�?B	��B	��B	�uB	y�B	aHB	I�B	9XB	+B	�B	bB��B�B��B��B�B�B�B�B�`B�
B��BŢBÖB�dB�RB�?B�B��B��B��B��B��B�VB�DB�B�B�B�%B�1B�%B�B�B{�Bu�Bs�Bn�Bl�BjBiyBiyBk�Bl�Bm�Bl�Bm�Bl�Br�Bt�Bv�Bw�B{�B�1B�hB�hB�oB�hB�%B�B�Bx�Bx�Bs�Bm�BjBbNB^5B[#BZBO�BI�BC�BB�B@�B:^B49B6FB5?B49B33B33B49B1'B1'B1'B0!B/B.B/B-B,B+B+B+B+B+B)�B)�B)�B,B+B+B+B+B)�B(�B)�B)�B(�B(�B(�B)�B+B+B+B,B,B,B,B,B49B;dB?}B?}B@�BC�BD�BD�BC�BC�BB�BC�BE�BF�BF�BH�BL�BN�BO�BQ�BR�BR�BW
BXBVBM�BF�BF�BI�BM�BS�BT�BT�BYB]/B`BBdZBe`BffBgmBgmBhsBhsBiyBhsBgmBgmBgmBhsBjBm�Bq�Bv�Bt�Bu�Bv�Bw�Bz�B{�B� B�%B�=B�=B�DB�VB�\B�bB�uB��B��B��B�bB�bB��B��B��B��B��B��B��B�!B�'B�-B�9B�?B�LB�RB�RB�RB�dB�wB�}BÖBƨB��B��B��B��B�B�/B�HB�`B�sB�B�B��B��B��B��B��B��B	  B	  B	B	B	B	%B	JB	bB	hB	uB	{B	�B	�B	�B	�B	�B	!�B	"�B	!�B	!�B	$�B	'�B	'�B	'�B	(�B	)�B	,B	1'B	7LB	:^B	:^B	;dB	=qB	@�B	A�B	@�B	B�B	F�B	I�B	J�B	J�B	L�B	M�B	N�B	O�B	P�B	T�B	YB	[#B	^5B	^5B	_;B	`BB	aHB	cTB	e`B	e`B	ffB	gmB	hsB	jB	l�B	n�B	s�B	u�B	y�B	|�B	� B	�B	�B	�B	�+B	�=B	�DB	�DB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�FB	�RB	�XB	�^B	�dB	�qB	�}B	�}B	�}B	�}B	B	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
+B
+B
1B
1B
1B
1B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
bB
hB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
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
.B
/B
/B
.B
/B
/B
/B
0!B
/B
/B
0!B
0!B
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
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
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
8RB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
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
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
L�B
M�B
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
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
VB
T�B
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
W
B
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
YB
YB
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
��B
�B
�UB
�gB
�tB
͟B_B#�B$�B0�BAUB[�Bt�B�"B�FB��B�tB�zB�tB�UB��B�EB�@B�MB�xB�pB��B�qB�BoiBT�B@OB;0B7B8B5B1�B,�B(�B%�BdB2B.B�B
��B
�B
�IB
�B
��B
�B
��B
��B
� B
��B
бB
�aB
��B
�B
�B
ncB
W�B
B[B
%�B	��B	�$B	ϑB	�B	��B	�dB	�@B	y�B	aB	I�B	9$B	*�B	YB	.B��B�B�B�nB�hB�UB�CB�0B�BּB�rB�SB�GB�B�B�B��B��B��B��B��B�eB�B��B��B��B��B��B��B��B��B��B{�ButBshBnIBlWBj0Bi*BiDBkQBl=BmCBl=BmCBl=BraBtnBvzBw�B{�B��B�B�B� B�B��B��B��Bx�Bx�BshBmCBj0Ba�B]�BZ�BY�BO�BI�BCGBBAB@4B:B3�B5�B4�B3�B2�B2�B3�B0�B0�B0�B/�B.�B-�B.�B,�B+�B*�B*�B*�B*�B*�B)�B)�B)�B+�B*�B*�B*�B*�B)�B(�B)�B)�B(�B(�B(�B)�B*�B*�B*�B+�B+�B+�B+�B+�B3�B;B?.B?.B@4BCGBDMBDMBCGBCGBBABCGBESBFYBFYBHKBL~BN�BO�BQ�BR�BR�BV�BW�BU�BM�BFYBFYBIlBM�BS�BT�BT�BX�B\�B_�BdBeBfBgBgBh$Bh$Bi*Bh$BgBgBgBh$Bj0Bm)BqABvzBtnButBvzBwfBz�B{�B�B��B��B��B��B�B�B�B�&B�QB�WB�9B�B�B�B�WB�CB�jB��B��B��B��B��B��B��B��B��B��B�B�B�B�(B�.B�GB�YB�dBѝBөB҉B��B��B��B�B�
B�IB�aB�tB�tB�zB�zB��B��B��B��B	 �B	�B	�B	�B	�B	�B	B	&B	,B	?B	1B	WB	]B	jB	!bB	"�B	!|B	!|B	$�B	'�B	'�B	'�B	(�B	)�B	+�B	0�B	6�B	9�B	9�B	;B	="B	@4B	A;B	@4B	BAB	FYB	IRB	JrB	JrB	L~B	M�B	N�B	O�B	P}B	T�B	X�B	Z�B	]�B	]�B	^�B	_�B	`�B	cB	d�B	d�B	fB	gB	h$B	j0B	l"B	n/B	shB	utB	y�B	|�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�&B	�,B	�9B	�?B	�?B	�?B	�KB	�KB	�QB	�dB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	��B	�"B	�.B	�B	�.B	�B	�'B	�SB	�YB	�_B	�fB	�fB	�fB	�lB	�xB	�~B	�dB	ΊB	�vB	�vB	�}B	ѝB	ңB	ӏB	өB	өB	ԕB	ԕB	ԯB	յB	՛B	ּB	ּB	ּB	ּB	֡B	��B	خB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�*B	�*B	�0B	�B	�6B	�B	�6B	�=B	�=B	�)B	�OB	�OB	�OB	�UB	�;B	�AB	�aB	�hB	�nB	�TB	�nB	�tB	�zB	��B	��B	�lB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
�B
B
B
B
�B
B
B
 B
 B
&B
&B
B
&B
B
,B
,B
,B
,B
,B
2B
2B
9B
9B
9B
9B
B
?B
+B
EB
EB
EB
EB
KB
KB
1B
7B
7B
WB
]B
]B
]B
]B
dB
dB
IB
dB
dB
IB
OB
dB
dB
OB
jB
OB
jB
pB
pB
 vB
 vB
 vB
!|B
!|B
!|B
!|B
"�B
"�B
"hB
#�B
#nB
#�B
#�B
#nB
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
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
-�B
.�B
.�B
-�B
.�B
.�B
.�B
/�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
9	B
8B
8B
7�B
9	B
8�B
9	B
9�B
;B
;B
:�B
:�B
:�B
<B
="B
="B
="B
>(B
="B
=B
=B
="B
="B
>(B
>(B
?.B
?.B
@4B
@4B
@B
@4B
@B
@4B
@4B
@4B
@4B
@4B
A B
A;B
A B
BAB
BAB
B'B
BAB
BAB
BAB
BAB
C-B
CGB
DMB
DMB
ESB
ESB
ESB
E9B
FYB
FYB
GEB
FYB
GEB
G_B
HfB
HKB
HfB
HfB
IRB
IlB
IlB
IlB
IlB
IlB
JrB
JrB
JrB
JrB
JrB
KxB
K^B
KxB
K^B
KxB
KxB
L~B
L~B
L~B
M�B
LdB
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
NpB
NpB
N�B
N�B
N�B
N�B
O�B
OvB
O�B
O�B
OvB
O�B
O�B
O�B
OvB
O�B
P}B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
U�B
T�B
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
V�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
X�B
X�B
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
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a�B
cB
cB
dB
dB
dB
dB
dB
c�B
dB
c�B
dB
dB
dB
dB
dB
d�B
eB
e�B
fB
fB
fB
fB
gB
gB
gB
gB
h$B
h
B
h$B
h$B
h
B
h
B
h
B
h$B
h
B
h$B
h$B
h$B
i*B
i*B
iB
i*B
i*B
iB
i*B
i*B
i*B
j0B
jB
j0B
k6B
k6B
l=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.64(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904130037372019041300373720190413003737201904140024392019041400243920190414002439JA  ARFMdecpA19c                                                                20190408033625  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190407183642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190407183643  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190407183644  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190407183644  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190407183644  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190407183644  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190407183644  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190407183645  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190407183645                      G�O�G�O�G�O�                JA  ARUP                                                                        20190407185527                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190408153429  CV  JULD            G�O�G�O�FťW                JM  ARCAJMQC2.0                                                                 20190412153737  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190412153737  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190413152439  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031518                      G�O�G�O�G�O�                