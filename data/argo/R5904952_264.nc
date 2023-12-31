CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:05Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190605  20181005190605  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�v��1   @��j <�N@2ȴ9X�c�ě��T1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�ff@�33A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bw��B�  B�  B�33B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D	  D	�fD
  D
� D
��D� D  D�fD  Dy�D��Dy�D  D� D��D� D  Dy�D  D� D  D�fD  D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D  Dy�D  D�fD  D� DfD� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3fD3�fD4  D4� D4��D5� D6  D6� D7  D7y�D8  D8�fD9  D9�fD:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DBy�DC  DCy�DD  DD� DEfDE�fDFfDF� DGfDG�fDH  DH�fDI  DI� DI��DJy�DK  DK� DL  DL� DM  DM� DNfDN�fDOfDO�fDP  DPy�DQ  DQ� DR  DR� DS  DS�fDTfDT�fDU  DUy�DV  DV� DW  DW� DX  DX� DY  DYy�DY��DZy�D[  D[�fD\  D\y�D]  D]�fD^  D^y�D_  D_�fD`fD`�fDafDa� Da��Dby�Db��Dc� Dd  Dd�fDefDe�fDf  Df� Dg  Dg� Dh  Dh� DifDi�fDj  Djy�Dk  Dk� Dl  Dl� Dm  Dm�fDnfDn� Dn��Do� Dp  Dp� Dp��Dq� Dr  Dry�Ds  Ds� Dt  Dt�fDu  Du� Dv  Dvy�Dw  Dw� Dw��Dy��D�K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�33A  A"ffAD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  BffB	  B  B  B!  B)  B1  B9  BA  BIffBQ  BY  Ba  Bi  Bq  Bx��B�� B�� B��3B��3B��3B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B��3B��3C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$Y�C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\&fC^&fC`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�3C�3C�  C�,�C�  C�  C�  C�  C�  C�3C�  C�,�C�,�C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�,�C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�  D  D � D D��D D� D D� D D� D D� D D� D D� D	�D��D	 D	�fD
 D
� D	�D� D D�fD D��D	�D��D D� D	�D� D D��D D� D D�fD D� D D� D D��D	�D��D D� D D� D D� D D��D D�fD D� DfD� D D�fD  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2�fD3fD3�fD4 D4� D5	�D5� D6 D6� D7 D7��D8 D8�fD9 D9�fD: D:� D; D;� D< D<��D= D=� D> D>� D? D?� D@ D@� DA DA� DB	�DB��DC DC��DD DD� DEfDE�fDFfDF� DGfDG�fDH DH�fDI DI� DJ	�DJ��DK DK� DL DL� DM DM� DNfDN�fDOfDO�fDP DP��DQ DQ� DR DR� DS DS�fDTfDT�fDU DU��DV DV� DW DW� DX DX� DY DY��DZ	�DZ��D[ D[�fD\ D\��D] D]�fD^ D^��D_ D_�fD`fD`�fDafDa� Db	�Db��Dc	�Dc� Dd Dd�fDefDe�fDf Df� Dg Dg� Dh Dh� DifDi�fDj Dj��Dk Dk� Dl Dl� Dm Dm�fDnfDn� Do	�Do� Dp Dp� Dq	�Dq� Dr Dr��Ds Ds� Dt Dt�fDu Du� Dv Dv��Dw Dw� Dw��Dy��D�S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�"�A� �A�(�A�(�A�+A�+A�+A�+A�-A�-A�-A�-A�/A�/A�1'A�1'A�33A�33A�33A�5?A�7LA�7LA�9XA�9XA�7LA�5?A�5?A�33A���Aʧ�A�x�A�bA�(�AƩ�AƍPA�hsA�`BA�ZA�9XA��A��HA�&�A�Q�A�A��\A���A���A���A��#A���A��DA�t�A���A��TA�
=A�A�A���A���A��A�O�A���A�hsA��hA�A�A�p�A���A���A�C�A�A�A�ƨA��;A�/A�
=A���A�9XA�M�A��`A���A�C�A��A��9A�x�A�"�A�ĜA�x�A��A��A�S�A���A�x�A�dZA��9A��A�+A�l�A�~�A�jA�%A��mA�7LA�+A~bA{�Ay��Axn�At{Al~�Ai�AgoAe�Aa��A_K�A]��A\�/A[&�AX�AUhsAQ33AO��AO7LAN �AK�-AJM�AG�;AC�FAAA>^5A=C�A:1'A9x�A8{A6z�A533A3�FA2��A21A1;dA0 �A/l�A,��A+K�A*5?A)/A(jA&��A%O�A"�A!��A!A!�;A!�A!&�A ZA�A�jAE�AdZAjAȴA  A��A�^A��A�7A�-AȴA7LA�HA�/A~�Ax�A�A1'A��AG�A�A(�AdZA�A�A�A
�A
I�A	dZA�A��AhsAȴAffA�7A�A+A�`A�DA��A ~�@��m@��@���@�t�@��@��@�@�1@��9@��9@�;d@�hs@� �@�@��H@��^@�33@�&�@�9X@�Q�@�  @�+@�ȴ@��#@�@�/@�1'@��@�J@�hs@�^5@�%@�h@陚@�@�=q@��H@�@�-@��@�j@��@�C�@�\@�%@�r�@�1'@�w@���@���@�?}@�7L@�Ĝ@�Q�@���@��
@�dZ@��@���@�n�@ݺ^@�Q�@�|�@�
=@�V@�J@ٲ-@���@�1'@��@׶F@�dZ@��@֗�@�V@���@պ^@��@��`@Ԭ@�r�@��@��
@�dZ@�"�@ҟ�@҇+@�n�@�-@�J@��@с@�7L@���@�Ĝ@�r�@��;@��@θR@�ff@�=q@�{@��@�p�@�@Η�@��@ΰ!@���@�p�@�Z@��;@�C�@ʏ\@�$�@���@���@��@���@ɲ-@Ɂ@�`B@�V@���@Ȭ@�\)@�ff@�`B@�&�@��/@ēu@�Z@�9X@��@��m@Õ�@�S�@�33@�+@�@�^5@�@���@��@�O�@�?}@�7L@���@��D@�\)@���@��@��T@���@��@�Ĝ@�z�@�(�@�
=@�v�@���@��^@��h@�X@�/@���@�1'@��P@�^5@�@���@�O�@���@�j@�b@��;@�t�@��R@�M�@���@��^@���@�G�@��/@��9@�r�@���@�|�@�dZ@�"�@�o@���@�ȴ@�~�@��@���@�X@�&�@��@��9@��@�z�@�I�@��
@��@���@���@�l�@��@��R@���@��+@�=q@��#@�hs@��@���@��9@��u@��w@�K�@�;d@�@���@��\@�=q@�{@��^@��/@���@��@�"�@�ȴ@�=q@��^@�/@�%@���@��@�b@�  @��@�S�@�\)@�;d@�ȴ@��!@�~�@�=q@�J@���@�V@�Ĝ@��@��u@�Z@� �@�S�@�ȴ@���@���@���@�p�@�X@��`@��w@�C�@�+@�@���@���@�E�@�@��#@�x�@�V@���@���@��9@�Q�@��;@�l�@�;d@�33@�
=@��@��H@��@��!@�-@�X@�1'@��P@�dZ@�@��@���@�v�@��@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�"�A� �A�(�A�(�A�+A�+A�+A�+A�-A�-A�-A�-A�/A�/A�1'A�1'A�33A�33A�33A�5?A�7LA�7LA�9XA�9XA�7LA�5?A�5?A�33A���Aʧ�A�x�A�bA�(�AƩ�AƍPA�hsA�`BA�ZA�9XA��A��HA�&�A�Q�A�A��\A���A���A���A��#A���A��DA�t�A���A��TA�
=A�A�A���A���A��A�O�A���A�hsA��hA�A�A�p�A���A���A�C�A�A�A�ƨA��;A�/A�
=A���A�9XA�M�A��`A���A�C�A��A��9A�x�A�"�A�ĜA�x�A��A��A�S�A���A�x�A�dZA��9A��A�+A�l�A�~�A�jA�%A��mA�7LA�+A~bA{�Ay��Axn�At{Al~�Ai�AgoAe�Aa��A_K�A]��A\�/A[&�AX�AUhsAQ33AO��AO7LAN �AK�-AJM�AG�;AC�FAAA>^5A=C�A:1'A9x�A8{A6z�A533A3�FA2��A21A1;dA0 �A/l�A,��A+K�A*5?A)/A(jA&��A%O�A"�A!��A!A!�;A!�A!&�A ZA�A�jAE�AdZAjAȴA  A��A�^A��A�7A�-AȴA7LA�HA�/A~�Ax�A�A1'A��AG�A�A(�AdZA�A�A�A
�A
I�A	dZA�A��AhsAȴAffA�7A�A+A�`A�DA��A ~�@��m@��@���@�t�@��@��@�@�1@��9@��9@�;d@�hs@� �@�@��H@��^@�33@�&�@�9X@�Q�@�  @�+@�ȴ@��#@�@�/@�1'@��@�J@�hs@�^5@�%@�h@陚@�@�=q@��H@�@�-@��@�j@��@�C�@�\@�%@�r�@�1'@�w@���@���@�?}@�7L@�Ĝ@�Q�@���@��
@�dZ@��@���@�n�@ݺ^@�Q�@�|�@�
=@�V@�J@ٲ-@���@�1'@��@׶F@�dZ@��@֗�@�V@���@պ^@��@��`@Ԭ@�r�@��@��
@�dZ@�"�@ҟ�@҇+@�n�@�-@�J@��@с@�7L@���@�Ĝ@�r�@��;@��@θR@�ff@�=q@�{@��@�p�@�@Η�@��@ΰ!@���@�p�@�Z@��;@�C�@ʏ\@�$�@���@���@��@���@ɲ-@Ɂ@�`B@�V@���@Ȭ@�\)@�ff@�`B@�&�@��/@ēu@�Z@�9X@��@��m@Õ�@�S�@�33@�+@�@�^5@�@���@��@�O�@�?}@�7L@���@��D@�\)@���@��@��T@���@��@�Ĝ@�z�@�(�@�
=@�v�@���@��^@��h@�X@�/@���@�1'@��P@�^5@�@���@�O�@���@�j@�b@��;@�t�@��R@�M�@���@��^@���@�G�@��/@��9@�r�@���@�|�@�dZ@�"�@�o@���@�ȴ@�~�@��@���@�X@�&�@��@��9@��@�z�@�I�@��
@��@���@���@�l�@��@��R@���@��+@�=q@��#@�hs@��@���@��9@��u@��w@�K�@�;d@�@���@��\@�=q@�{@��^@��/@���@��@�"�@�ȴ@�=q@��^@�/@�%@���@��@�b@�  @��@�S�@�\)@�;d@�ȴ@��!@�~�@�=q@�J@���@�V@�Ĝ@��@��u@�Z@� �@�S�@�ȴ@���@���@���@�p�@�X@��`@��w@�C�@�+@�@���@���@�E�@�@��#@�x�@�V@���@���@��9@�Q�@��;@�l�@�;d@�33@�
=@��@��H@��@��!@�-@�X@�1'@��P@�dZ@�@��@���@�v�@��@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B_;B_;Be`B��B�yB��B	"�B	@�B	S�B	hsB	m�B	p�B	�1B	ŢB
bB
8RB
cTB
�=B
�B
��BPBA�B�\B�B��BÖB�'B��B��B�jB�B�)B�/B�)B�B��B��B�JB}�BhsBW
B9XB33B �B
=BbB'�B$�B�B
=B
�B
�ZB
�TB
�NB
�;B
�#B
��B
��B
ȴB
�?B
��B
��B
~�B
e`B
Q�B
2-B	�B	�TB	�B	��B	��B	�^B	�FB	��B	�PB	� B	t�B	hsB	]/B	E�B	&�B	�B	\B	+B	B	1B	B	B��B�B�NB��B��B��B��B�#B�`B�B��B�}B�dB�qB�XB�FB�FB�LB�LB�LB�RB�XB�RB�FB�'B�?B�?B�?B�FB�LB�-B�!B�-B�RBƨB��B��BǮBĜB��BŢB��B�
B�B�#B�TB�yB�B��B��B��B	B	PB	uB	�B	�B	#�B	 �B	�B	�B	'�B	+B	-B	.B	.B	6FB	>wB	;dB	7LB	1'B	/B	-B	'�B	$�B	#�B	%�B	&�B	"�B	"�B	$�B	#�B	!�B	!�B	#�B	$�B	6FB	>wB	;dB	8RB	6FB	-B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	$�B	&�B	1'B	D�B	F�B	Q�B	jB	r�B	y�B	}�B	�%B	�DB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�3B	�?B	�FB	�LB	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�wB	�wB	��B	��B	��B	��B	��B	�}B	�wB	�wB	�wB	�}B	�wB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ƨB	��B	��B	�
B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
%B
%B
1B
	7B
	7B
DB
DB
JB
DB
JB
JB
DB
JB
JB
JB
JB
JB
JB
JB
PB
JB
DB
DB

=B

=B
	7B
1B
DB
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
oB
{B
�B
�B
�B
�B
�B
{B
{B
uB
oB
bB
VB
VB
VB
PB
PB
\B
B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B_;B_;Be`B��B�yB��B	"�B	@�B	S�B	hsB	m�B	p�B	�1B	ŢB
bB
8RB
cTB
�=B
�B
��BPBA�B�\B�B��BÖB�'B��B��B�jB�B�)B�/B�)B�B��B��B�JB}�BhsBW
B9XB33B �B
=BbB'�B$�B�B
=B
�B
�ZB
�TB
�NB
�;B
�#B
��B
��B
ȴB
�?B
��B
��B
~�B
e`B
Q�B
2-B	�B	�TB	�B	��B	��B	�^B	�FB	��B	�PB	� B	t�B	hsB	]/B	E�B	&�B	�B	\B	+B	B	1B	B	B��B�B�NB��B��B��B��B�#B�`B�B��B�}B�dB�qB�XB�FB�FB�LB�LB�LB�RB�XB�RB�FB�'B�?B�?B�?B�FB�LB�-B�!B�-B�RBƨB��B��BǮBĜB��BŢB��B�
B�B�#B�TB�yB�B��B��B��B	B	PB	uB	�B	�B	#�B	 �B	�B	�B	'�B	+B	-B	.B	.B	6FB	>wB	;dB	7LB	1'B	/B	-B	'�B	$�B	#�B	%�B	&�B	"�B	"�B	$�B	#�B	!�B	!�B	#�B	$�B	6FB	>wB	;dB	8RB	6FB	-B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	$�B	&�B	1'B	D�B	F�B	Q�B	jB	r�B	y�B	}�B	�%B	�DB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�3B	�?B	�FB	�LB	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�wB	�wB	��B	��B	��B	��B	��B	�}B	�wB	�wB	�wB	�}B	�wB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ƨB	��B	��B	�
B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
%B
%B
1B
	7B
	7B
DB
DB
JB
DB
JB
JB
DB
JB
JB
JB
JB
JB
JB
JB
PB
JB
DB
DB

=B

=B
	7B
1B
DB
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
oB
{B
�B
�B
�B
�B
�B
{B
{B
uB
oB
bB
VB
VB
VB
PB
PB
\B
B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190605                              AO  ARCAADJP                                                                    20181005190605    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190605  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190605  QCF$                G�O�G�O�G�O�8000            