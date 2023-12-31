CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:16Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190516  20181005190516  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               /A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׻$�ҏH1   @׻%/h^2@1��^5?}�c�S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      /A   A   A   @�33@�  A��A!��A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B��B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C��3D � D  D� D  D� D  Dy�D��Dy�D��Dy�D  D� D��Dy�D  D� D	  D	�fD
  D
� D  Dy�D  D� D  D�fDfD� D��D� DfD�fDfD�fDfD�fD  D� D  D� D  Dy�D  D� D  D�fDfD� D  Dy�D  D� D  D� D  D� D  Dy�D��D� D  Dy�D��D y�D!  D!� D"  D"y�D"��D#� D$  D$� D%fD%� D&fD&�fD'fD'�fD(  D(y�D)  D)�fD*  D*y�D+  D+� D+��D,y�D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2�fD3  D3� D4  D4�fD5fD5�fD6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DAy�DA��DBy�DB��DC� DDfDD�fDE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DI��DJ� DK  DK� DL  DL�fDM  DM� DNfDN� DO  DOy�DO��DPy�DP��DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\� D]  D]� D^fD^�fD_  D_y�D`  D`� D`��Da� Db  Dby�Db��Dcy�Dc��Dd� DefDe�fDffDf� Dg  Dg� DhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� DnfDn�fDofDo� DpfDp� DqfDq� Dr  Dr� Ds  Ds�fDt  Dty�Du  Du�fDv  Dv� Dw  Dw� Dw�fDy�=D�D{D�њ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A��A%��AD  Ad  A�  A�  A�  A���A�  A�  A�  A�  B  B	  B��B��B ��B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�L�B�� B�� B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3BĀ BȀ B̳3Bг3BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C &fC@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ CY�C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4Y�C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ&fC\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ CpY�Cr@ Ct&fCv@ Cx@ Cz@ C|@ C~@ C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�,�C�,�C�  C�  C�  C�  C�  C�,�C�  C�,�C�  C�3C�  C�  C�  C�  C�3C�3C�  C�,�C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�,�C�  C�  C�3C�3C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�  C�  C�  C�,�C�,�C�  D 	�D � D D� D D� D D��D	�D��D	�D��D D� D	�D��D D� D	 D	�fD
 D
� D D��D D� D D�fDfD� D	�D� DfD�fDfD�fDfD�fD D� D D� D D��D D� D D�fDfD� D D��D D� D D� D D� D D��D	�D� D D��D 	�D ��D! D!� D" D"��D#	�D#� D$ D$� D%fD%� D&fD&�fD'fD'�fD( D(��D) D)�fD* D*��D+ D+� D,	�D,��D-	�D-� D. D.� D/ D/� D0 D0� D1 D1� D2fD2�fD3 D3� D4 D4�fD5fD5�fD6 D6� D7 D7� D8fD8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA	�DA��DB	�DB��DC	�DC� DDfDD�fDE DE� DF DF� DG DG� DH	�DH� DI DI� DJ	�DJ� DK DK� DL DL�fDM DM� DNfDN� DO DO��DP	�DP��DQ	�DQ� DR DR� DS DS� DT DT� DU	�DU� DV DV� DW DW� DX DX� DY DY� DZ DZ�fD[ D[� D\ D\� D] D]� D^fD^�fD_ D_��D` D`� Da	�Da� Db Db��Dc	�Dc��Dd	�Dd� DefDe�fDffDf� Dg Dg� DhfDh�fDi Di� Dj Dj� Dk Dk� Dl Dl� Dm	�Dm� DnfDn�fDofDo� DpfDp� DqfDq� Dr Dr� Ds Ds�fDt Dt��Du Du�fDv Dv� Dw Dw� Dw�fDy�=D�L{D�ٚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A֝�A֣�A֥�A֧�A֣�A֩�A֧�A֧�A֩�A֧�A֩�A֩�A֩�A֮Aְ!A֬A֥�A֝�A֑hA�ffA�/Aղ-A�O�A�"�A�bA��TA���A԰!Aԣ�AԁA���A��TA�33AϑhA�ffA�v�A��A�dZA�C�A���A�5?A��;Aʥ�A�^5A�$�Aɟ�AȑhA�{A�~�A�C�A�ȴA���A�XA��A�ZAç�A���A���A�`BA���A�ȴA�~�A��A��A�I�A�+A�VA��mA���A�1A��
A�ffA��^A��TA��A�ȴA��yA�;dA���A��A�x�A��/A�oA�+A���A��+A�%A�dZA�1'A�E�A�ĜA��A���A���A��A��A��A�/A�jA�5?A��wA�C�A�A��9A�dZA���A�hsA�ȴA�C�A�A~��A{\)Ay��Ax�DAw�PAw&�Av�Au��Au"�As�wAq�TAn�!Ajn�Ag`BAd��Ab��A`�DA]��A\jAZ��AX��AU�AR�+AK��AG33AD��AB��A@�uA>��A=ƨA<A�A:=qA8��A5�7A2ȴA1��A1G�A0bNA/G�A.jA-G�A,��A,(�A+A)�A(��A(9XA't�A&z�A$�jA#XA!
=A��A��AbNAx�AVAoA\)A��A�
Av�Ap�A�AM�A
v�A�\AA|�A"�A�RAz�A�A;dAz�A��Av�Al�A��A=qA��A;dA ��@���@�E�@�?}@�A�@�S�@�J@��@���@�A�@���@�hs@��j@��
@���@�z�@�33@�7@� �@�w@�P@��@��@�G�@�@��y@�$�@�`B@�A�@㕁@���@�O�@��@߾w@�\)@��@�~�@���@��
@�@۶F@���@�&�@�j@���@ٙ�@�@��T@�hs@؛�@�9X@�"�@��@�=q@�@թ�@ԋD@�S�@ҟ�@�V@�O�@��@���@��/@��`@�j@�dZ@��@�-@�X@�b@�^5@�@��@�x�@��`@���@�A�@ǍP@�dZ@Ǿw@�1'@Ɵ�@š�@���@��@�b@�r�@���@�;d@��7@�?}@�V@�1'@�A�@�;d@��y@���@�
=@��@�;d@���@�z�@�?}@��@�@�@��\@��!@��@�1@��m@��
@��w@���@��@�dZ@��@���@�^5@���@���@��u@�9X@��F@�\)@��@��@��R@�n�@��@��-@�`B@���@��/@���@�9X@���@�33@��R@�ff@�M�@�M�@�$�@���@���@���@��@�1'@��;@�+@���@���@�E�@��@��@��^@���@��h@��h@��7@�x�@�hs@��@���@�z�@�"�@�@��7@�7L@�ƨ@���@�~�@�5?@��-@�X@�&�@��/@�%@�z�@��
@�Q�@���@���@�(�@���@��@�p�@�/@��9@��
@��P@�33@��F@��;@���@��D@�ȴ@��@��@�J@�^5@�@��!@���@��h@�@��+@��\@�V@�=q@��@��h@�G�@�hs@�hs@�hs@��h@�p�@�p�@�V@���@��/@�j@� �@��@�dZ@�+@�@���@�n�@�J@���@��7@�x�@�p�@�`B@�&�@��j@��@�z�@�b@�C�@���@�~�@��#@�@��^@���@�hs@�&�@�%@�Ĝ@��D@�Z@�(�@��@��F@���@�|�@�C�@�
=@���@�M�@���@�x�@�hs@�p�@�O�@��@�bN@�1'@���@��m@��
@��w@���@�\)@���@�n�@��@�z�@��m@���@�ƨ@��P@��H@�J@�hs@�O�@�7L@�V@��`@���@��m@�ƨ@��H@��\@�ff@�J@��#@��#@��#@�a@���@o��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A֝�A֣�A֥�A֧�A֣�A֩�A֧�A֧�A֩�A֧�A֩�A֩�A֩�A֮Aְ!A֬A֥�A֝�A֑hA�ffA�/Aղ-A�O�A�"�A�bA��TA���A԰!Aԣ�AԁA���A��TA�33AϑhA�ffA�v�A��A�dZA�C�A���A�5?A��;Aʥ�A�^5A�$�Aɟ�AȑhA�{A�~�A�C�A�ȴA���A�XA��A�ZAç�A���A���A�`BA���A�ȴA�~�A��A��A�I�A�+A�VA��mA���A�1A��
A�ffA��^A��TA��A�ȴA��yA�;dA���A��A�x�A��/A�oA�+A���A��+A�%A�dZA�1'A�E�A�ĜA��A���A���A��A��A��A�/A�jA�5?A��wA�C�A�A��9A�dZA���A�hsA�ȴA�C�A�A~��A{\)Ay��Ax�DAw�PAw&�Av�Au��Au"�As�wAq�TAn�!Ajn�Ag`BAd��Ab��A`�DA]��A\jAZ��AX��AU�AR�+AK��AG33AD��AB��A@�uA>��A=ƨA<A�A:=qA8��A5�7A2ȴA1��A1G�A0bNA/G�A.jA-G�A,��A,(�A+A)�A(��A(9XA't�A&z�A$�jA#XA!
=A��A��AbNAx�AVAoA\)A��A�
Av�Ap�A�AM�A
v�A�\AA|�A"�A�RAz�A�A;dAz�A��Av�Al�A��A=qA��A;dA ��@���@�E�@�?}@�A�@�S�@�J@��@���@�A�@���@�hs@��j@��
@���@�z�@�33@�7@� �@�w@�P@��@��@�G�@�@��y@�$�@�`B@�A�@㕁@���@�O�@��@߾w@�\)@��@�~�@���@��
@�@۶F@���@�&�@�j@���@ٙ�@�@��T@�hs@؛�@�9X@�"�@��@�=q@�@թ�@ԋD@�S�@ҟ�@�V@�O�@��@���@��/@��`@�j@�dZ@��@�-@�X@�b@�^5@�@��@�x�@��`@���@�A�@ǍP@�dZ@Ǿw@�1'@Ɵ�@š�@���@��@�b@�r�@���@�;d@��7@�?}@�V@�1'@�A�@�;d@��y@���@�
=@��@�;d@���@�z�@�?}@��@�@�@��\@��!@��@�1@��m@��
@��w@���@��@�dZ@��@���@�^5@���@���@��u@�9X@��F@�\)@��@��@��R@�n�@��@��-@�`B@���@��/@���@�9X@���@�33@��R@�ff@�M�@�M�@�$�@���@���@���@��@�1'@��;@�+@���@���@�E�@��@��@��^@���@��h@��h@��7@�x�@�hs@��@���@�z�@�"�@�@��7@�7L@�ƨ@���@�~�@�5?@��-@�X@�&�@��/@�%@�z�@��
@�Q�@���@���@�(�@���@��@�p�@�/@��9@��
@��P@�33@��F@��;@���@��D@�ȴ@��@��@�J@�^5@�@��!@���@��h@�@��+@��\@�V@�=q@��@��h@�G�@�hs@�hs@�hs@��h@�p�@�p�@�V@���@��/@�j@� �@��@�dZ@�+@�@���@�n�@�J@���@��7@�x�@�p�@�`B@�&�@��j@��@�z�@�b@�C�@���@�~�@��#@�@��^@���@�hs@�&�@�%@�Ĝ@��D@�Z@�(�@��@��F@���@�|�@�C�@�
=@���@�M�@���@�x�@�hs@�p�@�O�@��@�bN@�1'@���@��m@��
@��w@���@�\)@���@�n�@��@�z�@��m@���@�ƨ@��P@��H@�J@�hs@�O�@�7L@�V@��`@���@��m@�ƨ@��H@��\@�ff@�J@��#@��#@��#@�a@���@o��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
@�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
B�B
D�B
J�B
dZB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�ZB
��B
��B+B+B�B'�B1'B6FB;dB=qBH�Bo�B�B��B��B�B��B�B�BB�B��B�B(�B0!B8RB-B$�B#�B+BVBhsB� B��B��B��B�B�jB�dB�dB�RB�?B�-B�B��B��B��B�uB�DB�B|�Bq�Be`BQ�B"�B��B�TB�yB��B��B�TB��B��Bw�B-B
�/B
��B
�{B
B�B
"�B
#�B
 �B
\B	��B	�B	�yB	�B	��B	�sB	�fB	�TB	�NB	�5B	�/B	�B	��B	��B	��B	�VB	{�B	l�B	bNB	S�B	D�B	=qB	49B	(�B	�B	DB�B�5B�
B��B��B��BȴBǮBĜB��B�qB�dB�RB�FB�?B�3B�-B�-B�'B�'B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LBÖBŢB��B��B�qB��BÖB��BB��B�wB�qB�qB�qB�wB��B��B��B�}B�qB�jB�dB�dB�XB�^B�^B�jB�qB�qB�jB�qB�qB�wB�wB�}B��B��BĜBŢBĜBĜBŢBƨBƨBŢBĜBĜBŢBȴB��B�B�
B�#B�HB�B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	+B	%B	
=B	DB	PB	PB	oB	�B	�B	�B	oB	hB	�B	�B	�B	�B	&�B	(�B	(�B	'�B	'�B	,B	1'B	9XB	7LB	/B	(�B	-B	49B	;dB	8RB	49B	49B	5?B	8RB	=qB	=qB	=qB	@�B	F�B	I�B	L�B	O�B	W
B	\)B	_;B	aHB	cTB	iyB	p�B	t�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�%B	�+B	�=B	�DB	�DB	�DB	�DB	�DB	�JB	�JB	�PB	�PB	�VB	�\B	�bB	�uB	�{B	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�-B	�?B	�LB	�FB	�?B	�9B	�9B	�9B	�3B	�-B	�-B	�FB	�dB	��B	��B	ÖB	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	ǮB	ȴB	ɺB	��B	��B	�B	�B	��B	��B	��B	�B	�)B	�NB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
+B
%B
%B
%B
%B
%B
%B
%B
%B
+B
%B
+B
+B
+B
+B
+B
+B
%B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
�B
�B
'�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
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
@�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
B�B
D�B
J�B
dZB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�ZB
��B
��B+B+B�B'�B1'B6FB;dB=qBH�Bo�B�B��B��B�B��B�B�BB�B��B�B(�B0!B8RB-B$�B#�B+BVBhsB� B��B��B��B�B�jB�dB�dB�RB�?B�-B�B��B��B��B�uB�DB�B|�Bq�Be`BQ�B"�B��B�TB�yB��B��B�TB��B��Bw�B-B
�/B
��B
�{B
B�B
"�B
#�B
 �B
\B	��B	�B	�yB	�B	��B	�sB	�fB	�TB	�NB	�5B	�/B	�B	��B	��B	��B	�VB	{�B	l�B	bNB	S�B	D�B	=qB	49B	(�B	�B	DB�B�5B�
B��B��B��BȴBǮBĜB��B�qB�dB�RB�FB�?B�3B�-B�-B�'B�'B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LBÖBŢB��B��B�qB��BÖB��BB��B�wB�qB�qB�qB�wB��B��B��B�}B�qB�jB�dB�dB�XB�^B�^B�jB�qB�qB�jB�qB�qB�wB�wB�}B��B��BĜBŢBĜBĜBŢBƨBƨBŢBĜBĜBŢBȴB��B�B�
B�#B�HB�B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	+B	%B	
=B	DB	PB	PB	oB	�B	�B	�B	oB	hB	�B	�B	�B	�B	&�B	(�B	(�B	'�B	'�B	,B	1'B	9XB	7LB	/B	(�B	-B	49B	;dB	8RB	49B	49B	5?B	8RB	=qB	=qB	=qB	@�B	F�B	I�B	L�B	O�B	W
B	\)B	_;B	aHB	cTB	iyB	p�B	t�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�%B	�+B	�=B	�DB	�DB	�DB	�DB	�DB	�JB	�JB	�PB	�PB	�VB	�\B	�bB	�uB	�{B	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�-B	�?B	�LB	�FB	�?B	�9B	�9B	�9B	�3B	�-B	�-B	�FB	�dB	��B	��B	ÖB	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	ǮB	ȴB	ɺB	��B	��B	�B	�B	��B	��B	��B	�B	�)B	�NB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
+B
%B
%B
%B
%B
%B
%B
%B
%B
+B
%B
+B
+B
+B
+B
+B
+B
%B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
�B
�B
'�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190516                              AO  ARCAADJP                                                                    20181005190516    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190516  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190516  QCF$                G�O�G�O�G�O�8000            