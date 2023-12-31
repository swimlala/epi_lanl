CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  [   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        (D  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  mT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (D  wh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (D  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D \   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ,�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D 6�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (D ^�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �<   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (D �P   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (D è   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D P�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � x�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   y�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173027  20200828145439  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @׀�z�,@׀�z�,@׀�z�,111 @׀�}'ؐ@׀�}'ؐ@׀�}'ؐ@5��t�j@5��t�j@5��t�j�c�1&�y�c�1&�y�c�1&�y111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @@  @�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��D�@RD��RD�� D�=D�ND��
D��=D�3D�@�D�~Dǻ�D��D�I�DڍD�D��{D�)�D�iHD�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����ͽ���    ���ͽ���    ����        ����=���=��ͽ��ͽ���    ���ͽ��ͽ���    ����    ���ͽ��ͽ���=��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ���        ����    ���ͽ��ͽ��ͽ��ͽ���    ���ͽ���    ���ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ���=���    ����    =��ͽ��ͽ��ͽ���        ����        ����    =��ͽ��ͽ���        ���ͽ��ͽ���    ����    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���>L��>L��    ���ͽ���=���=��ͽ��ͽ���    ���ͽ���    ���ͽ��ͽ���    ���ͽ���    ���ͽ��ͽ���=���=��ͽ��ͽ��ͽ���    ����    ���ͽ���    ���ͽ���=���    ���ͽ��ͽ��ͽ���    ���ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ���=��ͽ��ͽ���=���=���=��ͽ��ͽ���        ���ͽ��ͽ��ͽ���=���    ����    ���ͽ���=���>L�ͽ��ͽ���=���=���    ����        =���=���    ����    ���ͽ���        ����    =���=��ͽ���    =��ͽ��ͽ���    ����    =���=���>L�ͽ���    ����=���        =���            =���        >L��    =���=���    =���=���    =���>L��>L��    =���>L��>L��>���>���>L��>L��>L��=���>L��>���>���=���>L��=���=���>L��>L��=���=���>L��>L��>L��>���    >L��>���>L��=���    =���>L��=���>L��=���>L��>���>L��=���>L��=���=���>L��=���>L��>L��    >L��=���=���>���>L��>L��=���>L��>L��>L��>L��>���=���>L��>L��>L��=���=���>���>L��>L��=���=���>L��>L��=���=���=���>L��>L��>L��>L��=���=���=���=���=���>L��>L��>L��>L��=���>L��>L��>L��=���=���>L��=���>L��=���=���>L��>L��=���=���    >L��>L��=���=���>L��>���>L��>L��>L��    >L��=���>L��>L��>L��>���>L��=���>L��>L��>L��>L��>L��    =���=���>L��>L��>���=���>L��=���>���>���>L��>L��>L��>L��=���=���>L��>L��=���>L��>L��>���>L��=���=���>L��>L��>L��>L��>L��>L��=���>L��>L��    =���>L��=���=���=���>L��=���>L��>L��=���=���=���>L��>L��=���=���>L��=���>L��>���>���>L��>���=���>L��>L��>L��=���>L��>���>���=���>L��>���>L��=���>L��>L��>L��>���>���>���=���=���    >L��>L��>���>L��=���>���>L��>���=���=���=���>L��=���    =���>L��>L��>���=���=���>L��=���>L��>L��>L��>L��>L��>���>���>���?   >���>L��    =���=���=���>���>L��=���=���>L��>���>���?   ?   ?   ?   ?��?333?333?333?fff?�  ?fff?�  ?���?���?�ff?�ff?�33?�33?�  ?�  ?���?���?ٙ�?�ff?�33?�33@   @   @ff@��@��@33@��@��@��@   @&ff@&ff@,��@333@9��@9��@@  @Fff@L��@S33@`  @`  @fff@l��@s33@y��@�  @�33@���@���@�  @�33@���@���@���@�33@�ff@���@���@�33@�33@���@�  @�  @�ff@ə�@���@�  @�ff@ٙ�@���@�  @�ff@陚@���@�  @�ff@���@���A   A��A33A��A  A	��A33A��AffA��A33A��AffA  A33A��AffA   A#33A$��A&ffA(  A+33A,��A.ffA0  A1��A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAQ��AS33AT��AVffAX  A[33A\��A^ffA`  Aa��Ac33Ad��Ah  Ai��Ak33Al��AnffAp  Aq��As33At��AvffAx  Ay��A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�ffA�  A���A͙�A�ffA�33A���Aљ�A�ffA�33A�  A���A�ffA�33A�  A���Aٙ�A�33A�  A���Aݙ�A�ffDq3Dq�Dq  Dq&fDq33Dq9�Dq@ DqFfDqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� @9��@@  @Fff@L��@S33@`  @`  @fff@l��@s33@y��@�  @�33@���@���@�  @�33@���@���@���@�33@�ff@���@���@�33@�33@���@�  @�  @�ff@ə�@���@�  @�ff@ٙ�@���@�  @�ff@陚@���@�  @�ff@���@���A   A��A33A��A  A	��A33A��AffA��A33A��AffA  A33A��AffA   A#33A$��A&ffA(  A+33A,��A.ffA0  A1��A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAQ��AS33AT��AVffAX  A[33A\��A^ffA`  Aa��Ac33Ad��Ah  Ai��Ak33Al��AnffAp  Aq��As33At��AvffAx  Ay��A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�ffA�  A���A͙�A�ffA�33A���Aљ�A�ffA�33A�  A���A�ffA�33A�  A���Aٙ�A�33A�  A���Aݙ�A�ffDq3Dq�Dq  Dq&fDq33Dq9�Dq@ DqFfDqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @8Q�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A��
A�
=A�
=A��
A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B���B��\B��\B�B�B�B���B�(�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC��C�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU��CW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg��DhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp��DqxRDq�RDrxRDr�RDsxRDs�RDtxRDy��D�D�<{D��{D��)D�fD�J=D��3D��fD�\D�=D�z=DǸ D��D�E�DډHD�HD��D�&D�eqD�o
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��aG��aG����aG��aG����aG������aG����	���	�aG��aG����aG��aG��aG����aG����aG��aG��aG����	�aG��aG��aG��aG��aG��aG��aG����aG��aG��aG������aG����aG��aG��aG��aG��aG����aG��aG����aG��aG����aG��aG��aG��aG��aG��aG����aG��aG����	���aG������	�aG��aG��aG������aG������aG������	�aG��aG������aG��aG��aG����aG����aG��aG��aG��aG��aG��aG��aG��aG�=��=�����aG��aG����	���	�aG��aG����aG��aG����aG��aG��aG����aG��aG����aG��aG��aG����	���	�aG��aG��aG����aG����aG��aG����aG��aG����	���aG��aG��aG��aG����aG��aG����aG��aG��aG��aG��aG��aG����aG��aG��aG����	�aG��aG����	���	���	�aG��aG������aG��aG��aG��aG����	���aG����aG��aG����	=���aG��aG����	���	���aG��������	���	���aG����aG��aG������aG������	���	�aG������	�aG��aG����aG������	���	=���aG����aG����	�������	���������	����=�������	���	�����	���	�����	=��=�������	=��=��>8Q�>8Q�=��=��=�����	=��>8Q�>8Q켣�	=�����	���	=��=�����	���	=��=��=��>8Q��=��>8Q�=�����	�����	=�����	=�����	=��>�\)=�����	=�����	���	=�����	=��=����=�����	���	>8Q�=��=�����	=��=��=��=��>8Q켣�	=��=��=�����	���	>�\)=��=�����	���	=��=�����	���	���	=��=��=��=�����	���	���	���	���	=��=��=��=�����	=��=��=�����	���	=�����	=�����	���	=��=�����	���	��=��=�����	���	=��>8Q�=��=��=����=�����	=��=��=��>8Q�=�����	=��=��=��=��=�������	���	=��=��>8Q켣�	=�����	>8Q�>8Q�=��=��=��=�����	���	=��=�����	=��=��>8Q�=�����	���	=��=��=��=��=��=�����	=��=�������	=�����	���	���	=�����	=��=�����	���	���	=��=�����	���	=�����	=��>�\)>8Q�=��>8Q켣�	=��=��=�����	=��>8Q�>8Q켣�	=��>8Q�=�����	=��=��=��>8Q�>8Q�>8Q켣�	���	��=��=��>8Q�=�����	>8Q�=��>8Q켣�	���	���	=�����	�����	=��=��>8Q켣�	���	=�����	=��=��=��=��=��>8Q�>8Q�>8Q�>\>8Q�=�������	���	���	>8Q�=�����	���	=��>8Q�>�\)>\>\>\>\>�?z�?z�?z�?G�?aG�?G�?aG�?z�H?�=q?�
=?�
=?��
?��
?���?���?�p�?�p�?�=q?�
=?��
?��
?��?��?�p�@�@�@�@�@�@�@Q�@�R@�R@%�@+�@1�@1�@8Q�@>�R@E�@K�@XQ�@XQ�@^�R@e�@k�@q�@xQ�@~�R@�@���@�(�@�\)@�@�@���@�\)@��\@�@���@�\)@�\)@�@�(�@�(�@\@�@���@�(�@ҏ\@�@���@�(�@�\@�@���@�(�@�\@�@�@�(�@�\*AG�A�HA{A�A	G�A
�HAz�A�AG�A�HAz�A{AG�A�HAz�A{A!G�A"�HA$z�A&{A)G�A*�HA,z�A.{A/�A2�HA4z�A6{A7�A9G�A<z�A>{A?�AAG�ADz�AF{AG�AIG�AJ�HALz�AO�AQG�AR�HATz�AV{AYG�AZ�HA\z�A^{A_�AaG�Ab�HAf{Ag�AiG�Aj�HAlz�An{Ao�AqG�Ar�HAtz�Av{Aw�Az�HA|z�A~{A�A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A���A���A�=pA�
=A��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aȣ�A�p�A�
=A��
Ạ�A�p�A�=pA��
AУ�A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aأ�A�=pA�
=A��
Aܣ�A�p�Dq�Dq�DqRDq�Dq+�Dq1�Dq8RDq>�DqK�DqQ�DqXRDq^�Dqk�Dqq�DqxRDq~�Dq��Dq��Dq�RDq��Dq��Dq��Dq�RDq��Dq˅Dq��Dq�RDq޸Dq�Dq��Dq�RDrDr�Dr�DrRDr%Dr+�Dr1�Dr8RDrEDrK�DrQ�DrXRDreDrk�Drq�DrxRDr�Dr��Dr��Dr�RDr�Dr��Dr��Dr��Dr�Dr˅Dr��Dr޸Dr�Dr�Dr�RDr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsEDsQ�DsXRDs^�Dsk�Dsq�DsxRDs~�Ds��Ds��Ds�RDs�Ds��Ds��Ds�RDs�Ds˅Ds��Ds޸Ds�Ds�Ds�RDs��DtDt�DtRDt�Dt%Dt1�Dt8RDt>�DtK�DtQ�DtXRDt^�Dtk�Dtq�DtxRDt�Dt��Dt��Dt�RDt�Dt��Dt��Dt��Dt�Dt˅Dt�R@1�@8Q�@>�R@E�@K�@XQ�@XQ�@^�R@e�@k�@q�@xQ�@~�R@�@���@�(�@�\)@�@�@���@�\)@��\@�@���@�\)@�\)@�@�(�@�(�@\@�@���@�(�@ҏ\@�@���@�(�@�\@�@���@�(�@�\@�@�@�(�@�\*AG�A�HA{A�A	G�A
�HAz�A�AG�A�HAz�A{AG�A�HAz�A{A!G�A"�HA$z�A&{A)G�A*�HA,z�A.{A/�A2�HA4z�A6{A7�A9G�A<z�A>{A?�AAG�ADz�AF{AG�AIG�AJ�HALz�AO�AQG�AR�HATz�AV{AYG�AZ�HA\z�A^{A_�AaG�Ab�HAf{Ag�AiG�Aj�HAlz�An{Ao�AqG�Ar�HAtz�Av{Aw�Az�HA|z�A~{A�A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A���A���A�=pA�
=A��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aȣ�A�p�A�
=A��
Ạ�A�p�A�=pA��
AУ�A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aأ�A�=pA�
=A��
Aܣ�A�p�Dq�Dq�DqRDq�Dq+�Dq1�Dq8RDq>�DqK�DqQ�DqXRDq^�Dqk�Dqq�DqxRDq~�Dq��Dq��Dq�RDq��Dq��Dq��Dq�RDq��Dq˅Dq��Dq�RDq޸Dq�Dq��Dq�RDrDr�Dr�DrRDr%Dr+�Dr1�Dr8RDrEDrK�DrQ�DrXRDreDrk�Drq�DrxRDr�Dr��Dr��Dr�RDr�Dr��Dr��Dr��Dr�Dr˅Dr��Dr޸Dr�Dr�Dr�RDr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsEDsQ�DsXRDs^�Dsk�Dsq�DsxRDs~�Ds��Ds��Ds�RDs�Ds��Ds��Ds�RDs�Ds˅Ds��Ds޸Ds�Ds�Ds�RDs��DtDt�DtRDt�Dt%Dt1�Dt8RDt>�DtK�DtQ�DtXRDt^�Dtk�Dtq�DtxRDt�Dt��Dt��Dt�RDt�Dt��Dt��Dt��Dt�Dt˅Dt�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aѥ�Aѧ�AѰ!AѰ!AѮAѬAѣ�Aч+A�jA�9XA���AН�AЛ�AЙ�AП�Aд9AжFAЗ�AЏ\A�~�A�l�A�XA�9XA�33A�1'A�1'A�5?A�9XA�=qA�C�A�Q�A�?}A��A�%A��TAϺ^AύPA�M�A�33A�VA�A��A���A�z�A�VA���A�+AÙ�A��yA�XA�bA�z�A��\A�ffA�-A���A��A��HA�M�A��jA�7LA���A�O�A��\A�ffA�jA��A��FA��
A���A��A��-A�1'A���A��mA�l�A�E�A�9XA�A�  A��HA��FA�%A�r�A��DA�ĜA�=qA��A�v�A��A��A���A��uA���A���A�ffA�E�A��A�|�A�dZA��\A�ƨA��A�{A�ĜA�-A|~�Ax�\Aw+AsoAm��Ak�
AjȴAh�DAhQ�AhE�AhJAfjAb��A_%A]O�A[�hAZE�AYK�AY&�AX�AW�wAVVAU|�AR�`APJAN��AM;dAL1'AJ�AJ$�AI��AI?}AH�HAH��AG�mAFffAC�mAB(�AAVA>�A=K�A;�hA:�/A:ffA:(�A9A9C�A81A7�A6�+A4�A3�TA2ffA1S�A-�FA+7LA)ƨA(Q�A'A%�mA#��A!��A!?}A �RA�A�A��A�!AI�Ax�AȴAdZAjAA��AbA�hA+A��A��A�TA��A(�A;dAA�A�A��An�A7LA�;A
Q�A	A1A?}A�AZAI�A$�AƨA;dA�\AA�AJA��A�mAVA33A ��A ��A ^5@���@�b@��F@�S�@�33@���@��y@�K�@��#@�7L@�I�@띲@ꟾ@���@�  @�;d@�@�n�@�%@�  @�n�@���@���@�+@���@�b@�^5@�/@֧�@�l�@ҸR@ҏ\@��@с@Ь@��;@���@���@Ͳ-@�V@���@�r�@��@�o@ʇ+@�ff@��@ɉ7@ț�@��y@�7L@��`@�r�@�ƨ@���@�E�@���@�hs@�t�@�5?@���@�x�@���@�z�@�o@��h@��@�K�@�E�@��\@�\)@�+@��R@��@���@�x�@�`B@�r�@� �@�  @�1'@�1@��m@���@�+@�-@�O�@�V@�V@�(�@�\)@�ff@�Z@�I�@�Z@�z�@�j@���@���@�33@��@��+@���@�x�@�&�@��/@���@�Q�@�9X@�(�@�1@��
@���@��;@�dZ@�+@��@��@�5?@��u@��;@��@�+@��@�
=@���@���@�~�@�5?@�@��h@�`B@�?}@���@���@��9@��D@�j@�I�@� �@��@��m@��;@��
@��F@�|�@�C�@���@�{@��T@��^@���@��@�p�@�hs@�G�@�/@�&�@��@�V@���@���@���@��@�j@�A�@�9X@� �@��@�b@���@��w@�dZ@�o@���@���@�~�@�ff@�V@�M�@�M�@�5?@�J@���@��-@�G�@�/@��@���@��@�Q�@�1'@�1@��@�ƨ@��@��P@��@�t�@�\)@�\)@�S�@�C�@�+@�n�@�$�@�@���@��@���@���@���@��h@�x�@�/@��@���@��@��`@���@�Ĝ@��D@�j@�9X@��w@���@�|�@�K�@��@�~�@�M�@���@�x�@�X@�O�@�O�@�X@�X@�O�@�?}@��@��@�Q�@�9X@��@�dZ@�33@��@�v�@�-@���@��7@�x�@�x�@�p�@�p�@�x�@�hs@�G�@�G�@�7L@�&�@�V@��9@�Q�@��m@��@�
=@��@��@�ȴ@���@�n�@�A�@r1�@n6�@b�L@X��@VM�@O�r@F�@<e�@4�@-<6@*}V@'��@"H�@�@l"@�"@?}@ \@
�+@�$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A��HAƾwA��HA�ĜA���A�%A�~�A�(�A�~�A�jA�p�AƧ�A���A���A��Aƙ�A�A�AƃA�-AǛ�A���A�^5A�p�AƝ�Aş�AƍPA�9XA��A�jAƟ�Aǝ�A���A���A�7LA��AƧ�Aě�A��;Aư!A���A�E�Aǥ�A���AȓuAȬA��A�1A�%A�A�Aǥ�A�`BAǰ!A�ƨA��yA�A�+A�bA���A���AɋDA�33A�;dA��A� �A�dZA��mA�A�9XAϩ�A���A��yA���Aƴ9A��A�VA�bA�^5A��A��yA��mA�C�A���AƼjA�XA���A�/Aȴ9A�1A�VA�S�AǛ�AȑhA�ƨA��AуA�|�A�?}AȓuA��`A�1'A�^5A���A�
=AɅAƋDA��/A�~�A�ffA�p�A��#A�
=A�jAƾwA�oA�33A�%A�A�Aд9AƟ�A�1A�hsA���A�"�AƃA�bNA�r�A��Aŉ7AȮA��
A�ȴA�x�A�hsA�7LA�1'A�ZAƣ�A��/A���A���A�1'A�/A��A�?}A�K�A���A���A�ȴAȑhA���A���A�5?A��`A�~�A�p�A��;A�XA�~�AƓuAƛ�A���A�5?A�z�AУ�A�JA�XA��/A�O�A�l�AήAщ7A�
=A�=qAͣ�AсAǬAȺ^A��mA��A��
A�v�A�S�Aǟ�A��#A�
=A�JA�M�A���AƲ-A�jA�A�ZAȕ�A�;dA�A�A��`A�z�A���Aư!A�dZA�  A�Aљ�A�oA���AʓuA���A�1'A��TAоwA�t�AΟ�A��/A��A�n�A��A�G�A���A��`AуA�1'AЋDAЇ+A�O�A�z�A�A�AѓuAа!A�ZA�
=Aї�AёhAћ�Aѝ�Aї�Aя\A�A�XAљ�Aѣ�Aї�A�z�A�$�Aћ�Aџ�Aџ�AГuAѓuAѝ�Aљ�Aѕ�Aџ�Aч+A���Aљ�Aѕ�AёhA��mA���A�?}AхAёhAхAэPAёhAћ�A�p�AуA�Q�A�%Aї�AэPA�  Aя\A�ffA�bNAэPA�?}AэPAя\AсAЗ�AхA�r�Aч+AхAѓuA�n�A��Aщ7Aщ7A�K�A�^5AсAёhAя\Aч+AыDA�M�Aѕ�AёhA�t�A�A�AэPAї�Aѕ�AѓuA�x�A�5?A���AёhAк^AѓuAя\AыDAѕ�AёhA�+AыDAэPA�jA͛�A���AэPAѓuA��/Aѕ�Aя\A�~�Aї�Aя\A�G�A���Aщ7A�v�A���Aя\Aї�Aћ�Aϛ�Aї�A�E�A���Aѕ�Aџ�A��HAљ�Aћ�Aћ�AёhAѝ�Aљ�Aѕ�Aѕ�AёhA�/A��Aѕ�Aѝ�Aщ7Aї�Aя\A���A�VAѝ�Aѝ�Aѝ�Aѝ�Aџ�Aѡ�A�v�A�hsA�C�Aћ�Aѝ�Aѝ�Aѝ�Aѝ�Aѕ�A�^5A�5?Aѝ�Aѝ�Aљ�Aѝ�Aї�Aљ�A�A�Aљ�Aї�Aя\A�M�Aї�A�n�Aѕ�A���AѓuA�O�Aї�AѓuAёhAͰ!AыDAёhAѕ�Aѕ�AΩ�AэPAэPA�+Aљ�Aѕ�A��
Aї�A�`BAѓuAѓuAХ�AѓuAя\AѓuAљ�A�A�AэPAѓuAћ�Aя\A�/Aя\AΡ�Aљ�Aљ�AѓuAї�Aɇ+Aͥ�A�9XAэPAч+Aѝ�AёhAя\Aљ�Aї�Aч+Aϟ�AэPAѓuAёhA�"�A�|�Aя\AэPAї�Aщ7A�hsA�?}A�~�A���Aя\AуA�~�AуAч+Aя\Aщ7Aџ�Aћ�AѓuA�dZA�v�A�1'A�S�AхAљ�AыDA�I�Aя\Aѝ�Aї�Aѡ�Aѝ�Aѝ�Aѧ�Aѥ�AѬAѥ�Aѣ�Aѣ�Aѥ�Aѣ�Aѡ�Aѧ�Aѥ�Aѧ�Aѩ�Aѩ�Aѩ�Aѩ�Aѧ�Aѩ�Aѩ�Aѧ�Aѧ�Aѩ�Aѩ�Aѧ�Aѥ�Aѧ�Aѩ�Aѩ�AѬAѧ�Aѩ�Aѥ�Aѣ�Aѥ�Aѣ�Aѣ�Aѥ�Aѣ�Aѣ�Aѣ�Aѣ�Aѣ�Aѣ�Aѧ�Aѥ�Aѧ�Aѣ�Aѥ�Aѧ�Aѧ�Aѩ�Aѩ�Aѧ�Aѩ�Aѩ�Aѩ�AѬAѩ�Aѩ�AѮAѰ!AѰ!AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѮAѲ-AѰ!AѰ!AѰ!AѮAѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѮAѰ!AѰ!AѲ-AѲ-AѮAѰ!AѰ!AѰ!AѰ!AѬAѮAѮAѰ!AѬAѮAѰ!AѬAѰ!AѰ!AѮAѮAѩ�Aѥ�Aѧ�Aѥ�Aѧ�Aѩ�AѮAѬAѥ�AѮAѬAѮAѮAѬAѬAѰ!AѰ!AѰ!AѮAѰ!AѰ!AѰ!Aѥ�Aѧ�Aџ�Aџ�Aѡ�Aѡ�Aѡ�Aѣ�Aѡ�Aѥ�Aѣ�Aѣ�Aѝ�Aћ�Aћ�Aџ�Aї�AыDAыDAя\Aя\AэPAэPAя\Aщ7AэPAыDA�z�AуA�t�A�z�A�|�A�|�AсA�~�A�z�A�v�A�v�A�p�A�n�A�r�A�n�A�p�A�p�A�n�A�n�A�hsA�VA�\)A�\)A�^5A�`BA�`BA�`BA�dZA�Q�A�K�A�O�A�I�A�=qA�;dA�9XA�-A�&�A�(�A�$�A��A�oA�VA�oA�
=A�
=A�1A�%A���A���A��mA��HA���A�ĜAк^AиRAжFAд9AЮAЬAЧ�AЩ�AХ�AХ�AУ�AС�AЛ�AЛ�AН�AН�AЛ�AЛ�AЛ�AЙ�AЙ�AЗ�AЙ�AЙ�AЙ�AЙ�AЛ�AЙ�AЗ�AЛ�AЛ�AЛ�AЙ�AЙ�AЛ�AЛ�AЛ�AЛ�AЛ�AЛ�AЙ�AЛ�AН�AЛ�AН�AЛ�AЙ�AЛ�@��
@���@���@���@��@��@���@���@���@���@���@���@���@���@��P@��P@��P@�|�@�dZ@�\)@�K�@�C�@�33@�+@�"�@��@��@�o@�@�@�@�@���@���@�@�@���@���@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��H@��@��@��@��@��@��@��@���@���@�ȴ@�ȴ@�ȴ@�ȴ@���@�ȴ@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@�ȴ@���@�ȴ@���@���@���@���@���@��R@��!@���@���@���@��\@��\@�~�@�v�@�v�@�v�@�v�@�v�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�ff@�^5@�^5@�V@�V@�V@�V@�=qAѥ�Aѥ�Aѣ�Aѧ�Aѥ�Aѧ�Aѥ�Aѥ�Aѥ�Aѧ�Aѧ�Aѩ�Aѩ�Aѩ�Aѩ�Aѩ�AѬAѩ�AѬAѮAѰ!AѰ!AѲ-AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѰ!AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѮAѲ-AѲ-AѰ!AѲ-AѰ!AѲ-AѮAѮAѮAѮAѰ!AѰ!AѮAѬAѬAѧ�Aѧ�Aѧ�AѬAѥ�AѬAѧ�AѬAѰ!AѮAѬAѬAѬAѰ!AѰ!AѲ-AѮAѰ!AѮAѮAѰ!AѬAѧ�Aѡ�Aѡ�Aѡ�Aѡ�Aѣ�Aѣ�Aѣ�Aѣ�Aѡ�Aѣ�Aџ�Aї�Aї�Aџ�Aї�Aѕ�AэPAэPAэPAыDAя\Aя\AёhAэPAхAхA�|�A�p�A�r�A�t�A�|�A�~�A�~�A�v�A�r�A�t�A�p�A�p�A�n�A�p�A�p�A�p�A�l�A�l�A�bNA�VA�\)A�^5A�^5A�^5A�\)A�\)A�`BA�O�A�O�A�Q�A�E�A�E�A�;dA�1'A�-A�(�A�(�A�$�A� �A�oA�bA�JA�1A�
=A�JA�%A�  A��A��`A���A�ĜAоwAк^AиRAжFAа!AЮAЬAЧ�AЧ�AХ�AУ�AС�AП�AН�AН�AН�AЛ�AН�AЛ�AЙ�AЙ�AЙ�AЙ�AЙ�AЙ�AЙ�AЛ�AЙ�AЛ�AЙ�AЛ�AЙ�AЛ�AЛ�AЛ�AЛ�AЛ�AЙ�AЛ�AЛ�AЙ�AЛ�AЙ�AН�AН�AН�AЛ�AЛ�AЛ�@��
@���@���@��F@��F@��@���@���@���@���@���@���@���@��P@��P@��@��@�t�@�dZ@�\)@�K�@�;d@�33@�"�@�"�@��@�o@�
=@�@�@�@�@���@���@���@���@���@���@��@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��@��@��@��@��@��@��@���@���@���@���@�ȴ@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@�ȴ@�ȴ@���@���@���@���@���@��R@��R@��!@���@���@���@��\@��+@�v�@�v�@�v�@�v�@�v�@�v�@�n�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�V@�V@�V@�M�@�5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   Aѥ�Aѧ�AѰ!AѰ!AѮAѬAѣ�Aч+A�jA�9XA���AН�AЛ�AЙ�AП�Aд9AжFAЗ�AЏ\A�~�A�l�A�XA�9XA�33A�1'A�1'A�5?A�9XA�=qA�C�A�Q�A�?}A��A�%A��TAϺ^AύPA�M�A�33A�VA�A��A���A�z�A�VA���A�+AÙ�A��yA�XA�bA�z�A��\A�ffA�-A���A��A��HA�M�A��jA�7LA���A�O�A��\A�ffA�jA��A��FA��
A���A��A��-A�1'A���A��mA�l�A�E�A�9XA�A�  A��HA��FA�%A�r�A��DA�ĜA�=qA��A�v�A��A��A���A��uA���A���A�ffA�E�A��A�|�A�dZA��\A�ƨA��A�{A�ĜA�-A|~�Ax�\Aw+AsoAm��Ak�
AjȴAh�DAhQ�AhE�AhJAfjAb��A_%A]O�A[�hAZE�AYK�AY&�AX�AW�wAVVAU|�AR�`APJAN��AM;dAL1'AJ�AJ$�AI��AI?}AH�HAH��AG�mAFffAC�mAB(�AAVA>�A=K�A;�hA:�/A:ffA:(�A9A9C�A81A7�A6�+A4�A3�TA2ffA1S�A-�FA+7LA)ƨA(Q�A'A%�mA#��A!��A!?}A �RA�A�A��A�!AI�Ax�AȴAdZAjAA��AbA�hA+A��A��A�TA��A(�A;dAA�A�A��An�A7LA�;A
Q�A	A1A?}A�AZAI�A$�AƨA;dA�\AA�AJA��A�mAVA33A ��A ��A ^5@���@�b@��F@�S�@�33@���@��y@�K�@��#@�7L@�I�@띲@ꟾ@���@�  @�;d@�@�n�@�%@�  @�n�@���@���@�+@���@�b@�^5@�/@֧�@�l�@ҸR@ҏ\@��@с@Ь@��;@���@���@Ͳ-@�V@���@�r�@��@�o@ʇ+@�ff@��@ɉ7@ț�@��y@�7L@��`@�r�@�ƨ@���@�E�@���@�hs@�t�@�5?@���@�x�@���@�z�@�o@��h@��@�K�@�E�@��\@�\)@�+@��R@��@���@�x�@�`B@�r�@� �@�  @�1'@�1@��m@���@�+@�-@�O�@�V@�V@�(�@�\)@�ff@�Z@�I�@�Z@�z�@�j@���@���@�33@��@��+@���@�x�@�&�@��/@���@�Q�@�9X@�(�@�1@��
@���@��;@�dZ@�+@��@��@�5?@��u@��;@��@�+@��@�
=@���@���@�~�@�5?@�@��h@�`B@�?}@���@���@��9@��D@�j@�I�@� �@��@��m@��;@��
@��F@�|�@�C�@���@�{@��T@��^@���@��@�p�@�hs@�G�@�/@�&�@��@�V@���@���@���@��@�j@�A�@�9X@� �@��@�b@���@��w@�dZ@�o@���@���@�~�@�ff@�V@�M�@�M�@�5?@�J@���@��-@�G�@�/@��@���@��@�Q�@�1'@�1@��@�ƨ@��@��P@��@�t�@�\)@�\)@�S�@�C�@�+@�n�@�$�@�@���@��@���@���@���@��h@�x�@�/@��@���@��@��`@���@�Ĝ@��D@�j@�9X@��w@���@�|�@�K�@��@�~�@�M�@���@�x�@�X@�O�@�O�@�X@�X@�O�@�?}@��@��@�Q�@�9X@��@�dZ@�33@��@�v�@�-@���@��7@�x�@�x�@�p�@�p�@�x�@�hs@�G�@�G�@�7L@�&�@�V@��9@�Q�@��m@��@�
=@��@��@�ȴ@���G�O�@�A�@r1�@n6�@b�L@X��@VM�@O�r@F�@<e�@4�@-<6@*}V@'��@"H�@�@l"@�"@?}@ \@
�+@�$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A��HAƾwA��HA�ĜA���A�%A�~�A�(�A�~�A�jA�p�AƧ�A���A���A��Aƙ�A�A�AƃA�-AǛ�A���A�^5A�p�AƝ�Aş�AƍPA�9XA��A�jAƟ�Aǝ�A���A���A�7LA��AƧ�Aě�A��;Aư!A���A�E�Aǥ�A���AȓuAȬA��A�1A�%A�A�Aǥ�A�`BAǰ!A�ƨA��yA�A�+A�bA���A���AɋDA�33A�;dA��A� �A�dZA��mA�A�9XAϩ�A���A��yA���Aƴ9A��A�VA�bA�^5A��A��yA��mA�C�A���AƼjA�XA���A�/Aȴ9A�1A�VA�S�AǛ�AȑhA�ƨA��AуA�|�A�?}AȓuA��`A�1'A�^5A���A�
=AɅAƋDA��/A�~�A�ffA�p�A��#A�
=A�jAƾwA�oA�33A�%A�A�Aд9AƟ�A�1A�hsA���A�"�AƃA�bNA�r�A��Aŉ7AȮA��
A�ȴA�x�A�hsA�7LA�1'A�ZAƣ�A��/A���A���A�1'A�/A��A�?}A�K�A���A���A�ȴAȑhA���A���A�5?A��`A�~�A�p�A��;A�XA�~�AƓuAƛ�A���A�5?A�z�AУ�A�JA�XA��/A�O�A�l�AήAщ7A�
=A�=qAͣ�AсAǬAȺ^A��mA��A��
A�v�A�S�Aǟ�A��#A�
=A�JA�M�A���AƲ-A�jA�A�ZAȕ�A�;dA�A�A��`A�z�A���Aư!A�dZA�  A�Aљ�A�oA���AʓuA���A�1'A��TAоwA�t�AΟ�A��/A��A�n�A��A�G�A���A��`AуA�1'AЋDAЇ+A�O�A�z�A�A�AѓuAа!A�ZA�
=Aї�AёhAћ�Aѝ�Aї�Aя\A�A�XAљ�Aѣ�Aї�A�z�A�$�Aћ�Aџ�Aџ�AГuAѓuAѝ�Aљ�Aѕ�Aџ�Aч+A���Aљ�Aѕ�AёhA��mA���A�?}AхAёhAхAэPAёhAћ�A�p�AуA�Q�A�%Aї�AэPA�  Aя\A�ffA�bNAэPA�?}AэPAя\AсAЗ�AхA�r�Aч+AхAѓuA�n�A��Aщ7Aщ7A�K�A�^5AсAёhAя\Aч+AыDA�M�Aѕ�AёhA�t�A�A�AэPAї�Aѕ�AѓuA�x�A�5?A���AёhAк^AѓuAя\AыDAѕ�AёhA�+AыDAэPA�jA͛�A���AэPAѓuA��/Aѕ�Aя\A�~�Aї�Aя\A�G�A���Aщ7A�v�A���Aя\Aї�Aћ�Aϛ�Aї�A�E�A���Aѕ�Aџ�A��HAљ�Aћ�Aћ�AёhAѝ�Aљ�Aѕ�Aѕ�AёhA�/A��Aѕ�Aѝ�Aщ7Aї�Aя\A���A�VAѝ�Aѝ�Aѝ�Aѝ�Aџ�Aѡ�A�v�A�hsA�C�Aћ�Aѝ�Aѝ�Aѝ�Aѝ�Aѕ�A�^5A�5?Aѝ�Aѝ�Aљ�Aѝ�Aї�Aљ�A�A�Aљ�Aї�Aя\A�M�Aї�A�n�Aѕ�A���AѓuA�O�Aї�AѓuAёhAͰ!AыDAёhAѕ�Aѕ�AΩ�AэPAэPA�+Aљ�Aѕ�A��
Aї�A�`BAѓuAѓuAХ�AѓuAя\AѓuAљ�A�A�AэPAѓuAћ�Aя\A�/Aя\AΡ�Aљ�Aљ�AѓuAї�Aɇ+Aͥ�A�9XAэPAч+Aѝ�AёhAя\Aљ�Aї�Aч+Aϟ�AэPAѓuAёhA�"�A�|�Aя\AэPAї�Aщ7A�hsA�?}A�~�A���Aя\AуA�~�AуAч+Aя\Aщ7Aџ�Aћ�AѓuA�dZA�v�A�1'A�S�AхAљ�AыDA�I�Aя\Aѝ�Aї�Aѡ�Aѝ�Aѝ�Aѧ�Aѥ�AѬAѥ�Aѣ�Aѣ�Aѥ�Aѣ�Aѡ�Aѧ�Aѥ�Aѧ�Aѩ�Aѩ�Aѩ�Aѩ�Aѧ�Aѩ�Aѩ�Aѧ�Aѧ�Aѩ�Aѩ�Aѧ�Aѥ�Aѧ�Aѩ�Aѩ�AѬAѧ�Aѩ�Aѥ�Aѣ�Aѥ�Aѣ�Aѣ�Aѥ�Aѣ�Aѣ�Aѥ�Aѥ�Aѣ�Aѧ�Aѥ�Aѧ�Aѥ�Aѥ�Aѥ�Aѧ�Aѧ�Aѩ�Aѩ�Aѩ�Aѩ�Aѩ�AѬAѩ�AѬAѮAѰ!AѰ!AѲ-AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѰ!AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѮAѲ-AѲ-AѰ!AѲ-AѰ!AѲ-AѮAѮAѮAѮAѰ!AѰ!AѮAѬAѬAѧ�Aѧ�Aѧ�AѬAѥ�AѬAѧ�AѬAѰ!AѮAѬAѬAѬAѰ!AѰ!AѲ-AѮAѰ!AѮAѮAѰ!AѬAѧ�Aѡ�Aѡ�Aѡ�Aѡ�Aѣ�Aѣ�Aѣ�Aѣ�Aѡ�Aѣ�Aџ�Aї�Aї�Aџ�Aї�Aѕ�AэPAэPAэPAыDAя\Aя\AёhAэPAхAхA�|�A�p�A�r�A�t�A�|�A�~�A�~�A�v�A�r�A�t�A�p�A�p�A�n�A�p�A�p�A�p�A�l�A�l�A�bNA�VA�\)A�^5A�^5A�^5A�\)A�\)A�`BA�O�A�O�A�Q�A�E�A�E�A�;dA�1'A�-A�(�A�(�A�$�A� �A�oA�bA�JA�1A�
=A�JA�%A�  A��A��`A���A�ĜAоwAк^AиRAжFAа!AЮAЬAЧ�AЧ�AХ�AУ�AС�AП�AН�AН�AН�AЛ�AН�AЛ�AЙ�AЙ�AЙ�AЙ�AЙ�AЙ�AЙ�AЛ�AЙ�AЛ�AЙ�AЛ�AЙ�AЛ�AЛ�AЛ�AЛ�AЛ�AЙ�AЛ�AЛ�AЙ�AЛ�AЙ�AН�AН�AН�AЛ�AЛ�AЛ�@��
@���@���@��F@��F@��@���@���@���@���@���@���@���@��P@��P@��@��@�t�@�dZ@�\)@�K�@�;d@�33@�"�@�"�@��@�o@�
=@�@�@�@�@���@���@���@���@���@���@��@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��@��@��@��@��@��@��@���@���@���@���@�ȴ@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@�ȴ@�ȴ@���@���@���@���@���@��R@��R@��!@���@���@���@��\@��+@�v�@�v�@�v�@�v�@�v�@�v�@�n�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�V@�V@�V@�M�@�5?Aѥ�Aѥ�Aѣ�Aѧ�Aѥ�Aѧ�Aѥ�Aѥ�Aѥ�Aѧ�Aѧ�Aѩ�Aѩ�Aѩ�Aѩ�Aѩ�AѬAѩ�AѬAѮAѰ!AѰ!AѲ-AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѰ!AѲ-AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѰ!AѲ-AѰ!AѮAѲ-AѲ-AѰ!AѲ-AѰ!AѲ-AѮAѮAѮAѮAѰ!AѰ!AѮAѬAѬAѧ�Aѧ�Aѧ�AѬAѥ�AѬAѧ�AѬAѰ!AѮAѬAѬAѬAѰ!AѰ!AѲ-AѮAѰ!AѮAѮAѰ!AѬAѧ�Aѡ�Aѡ�Aѡ�Aѡ�Aѣ�Aѣ�Aѣ�Aѣ�Aѡ�Aѣ�Aџ�Aї�Aї�Aџ�Aї�Aѕ�AэPAэPAэPAыDAя\Aя\AёhAэPAхAхA�|�A�p�A�r�A�t�A�|�A�~�A�~�A�v�A�r�A�t�A�p�A�p�A�n�A�p�A�p�A�p�A�l�A�l�A�bNA�VA�\)A�^5A�^5A�^5A�\)A�\)A�`BA�O�A�O�A�Q�A�E�A�E�A�;dA�1'A�-A�(�A�(�A�$�A� �A�oA�bA�JA�1A�
=A�JA�%A�  A��A��`A���A�ĜAоwAк^AиRAжFAа!AЮAЬAЧ�AЧ�AХ�AУ�AС�AП�AН�AН�AН�AЛ�AН�AЛ�AЙ�AЙ�AЙ�AЙ�AЙ�AЙ�AЙ�AЛ�AЙ�AЛ�AЙ�AЛ�AЙ�AЛ�AЛ�AЛ�AЛ�AЛ�AЙ�AЛ�AЛ�AЙ�AЛ�AЙ�AН�AН�AН�AЛ�AЛ�AЛ�@��
@���@���@��F@��F@��@���@���@���@���@���@���@���@��P@��P@��@��@�t�@�dZ@�\)@�K�@�;d@�33@�"�@�"�@��@�o@�
=@�@�@�@�@���@���@���@���@���@���@��@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��@��@��@��@��@��@��@���@���@���@���@�ȴ@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@�ȴ@�ȴ@���@���@���@���@���@��R@��R@��!@���@���@���@��\@��+@�v�@�v�@�v�@�v�@�v�@�v�@�n�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�V@�V@�V@�M�@�5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=[�=`=n�p=���=���=�˒>ۡ?�1=̷�>6�L@� G>�[�=G�=V�L=m��=�If=�A_=���=��>)$�=�g�=�#=��>d�>�N= ��=%e�=+�]=&��=*�"='2�=M�u=eU�=��=�?=�ی>�=wQ=�H=)i=9��=Kf=l��=w<=�4=�Q�>�}?���=4$_=eU�=���=��=T�=.4=/n�=:�=MU=Z��=n��=�Y�=�.�>x�=��2=��>LV�@�.�=���=���>�?�z=rf�=�̎=�aR=���>(�+?��?nF�=a@=j��=��x=���=���=�Sz>�!>�(x=��%=L�P=g��=R��=tS�=�QY=�'�=��j=��>Z��@�H@�&=돛=ښ�>Q$J@�H>6��=��e>4�?�@=��=&77=4Y=:�=6Z�=Tk'=U�=s�=���=�]=��A=�Yu>э@��=h�=qk=vJ�=���=�j�=�Pr=�nn=�e=�(N=���=��>��Q?�_�=�=��=�=�9�> 1�=;�=Dڐ=U��=fff=p�b=�1�=�:*=�[-=�5�>'Z2?<��=�W�>��?�ֶ?�>iY>l��@�:~@�1'=W��=~q�>
�=^��=u�=��~=��> �?q��@2y=�m	?��=�;�=��$?];�@�7�@"o?=�O�>4��@�:~=��=�Ѣ>��?��~>U�@�.4@�28=���>^�K@[��=�\>M��@�:�=�^_=۰`>_�T@���?��Z>��M@#�[=��f>��?�=G=��v=���>1[W@`!�@�?�?��=��=��.>��Y?�K?�E�?���@�@�@H�?�K4>���=�9�>�@nI�@��!>5�@�?}?o�7?��'@D��>G��@�CB?vݭ@�@:@�DR@k��>Qm�@�B@�B@�E�@�B@�B@�>�@L�w@g�@�C�@�F�@�C�?���@�B@�<�@�D|@�C�@Blv@�C�@��@�B�@�E�@�E9@�D(?��8@�AJ@�C�@�B?�r?n��@�:*@�@�@�?�@�<�@�>l@�B�@�C�@�@�@��z@�=�@7"@�B@�?}?��@�=�@�?�@l4D@�<�@�=@�<6@�>�@�:�@��@�:�@�:�@�;�@�:�@�>�@�:*@!�u@�<�@�=\@�;�@pY!@�;�@�@:@�<6@�?)@�<�@�?�@�?}@�@�>ר�@�N@�?�@�At@�?>@�@:@�5�?%E@'��@�>l@�W�@�>�@�=@�At@�@:@�@:?e�^@�@:@�?)@�<6@��@���@�@:@�?�?p�Y@�@�@�=\@�<�@�At@�@O@h��>BU�@�@:@�@:@��@�A�@�B@�B�?��@�B�@�A_>��@�B�@�B[@��@�B�@�B�@�C@�B@�B[@�AJ@�@:@�?�@�<�@[x�@b�4@��D@�B�@�B�@�B�@�CB@�0�@�>-@�C�@�C�@�C�@�C�@�C�@�C�@�D(@�!?���@�C�@�C�@�B@�B[@�B[@�B[@�B�?#@�C@�D|@�C�@�C�@�B�@�B[>��X@�A�@�A�@�?)>�4�@�B�@�A�@�@:@k��@�?�@�AJ@�A�@�A@�@:@4�@�=�@�>�@�@�@�AJ>��)@�?)@�AJ@�A�@�A�@�AJ@���@�At@�>�@�?�@�>�@g�;@�@:@�?)@�A@�B�@�@:@�@�@�B[@�C�@�B�?�@�AJ?U3�@�C�@�B@�@�@�D(>��7?� G@�.@�?}@�=\@�C�@�A�@�@�@�A�@�@O@�AJ?S��@�?}@�?}@�?�@�>�?'T�@�?�@�?}@�?}@�?�>��o?�(N@�>l@X��@�?}@�;:@�:*@�;:@�<6@�=�@�=�@�C�@�D(@�AJ@�?�=��N?΅@�@@�I(@�B�@�@�@_�'@�E�@�F_@�B�@�F�@�E�@�H@�G�@�H@�G�@�G�@�F�@�F�@�Go@�Go@�Go@�H@�H�@�H@�I(@�H�@�H�@�H�@�H�@�H�@�H�@�H�@�H�@�G�@�HV@�H,@�Go@�G�@�I�@�H�@�H�@�HV@�GE@�G@�Go@�Go@�F�@�G@�GE@�GE@�G�@�GE@�Go@�G�@�H�@�H�@�H�@�H,@�H,@�H�@�I=@�I�@�I�@�JM@�J�@�K
@�K^@�K�@�L@�K�@�L�@�M@�Oa@�O�@�P@�N�@�N�@�N�@�O�@�O�@�O�@�P@�O�@�N�@�O�@�N�@�N�@�O@�Oa@�O�@�P@�P@�P@�Pr@�Pr@�Pr@�Q�@�Q�@�Q�@�Q�@�R�@�R�@�R�@�R�@�R�@�R�@�SP@�R�@�Se@�SP@�Se@�Se@�SP@�Se@�Se@�S�@�S�@�S�@�S�@�S�@�T"@�T"@�S�@�T@�S�@�Tv@�T�@�T�@�T@�Tv@�Tv@�U2@�T�@�T�@�Tv@�U2@�T�@�U2@�T�@�T�@�T"@�T�@�T�@�T�@�T"@�T�@�T�@�T�@�T�@�T�@�T�@�S�@�S�@�T"@�S�@�Sz@�S@�S@�S@�R�@�S@�S@�S@�S@�S@�R@�Q�@�QD@�P�@�QY@�Q�@�P�@�P�@�P�@�P�@�PH@�O�@�O�@�O�@�O�@�Ov@�O"@�O"@�M�@�N{@�M�@�L�@�L�@�M@�Mj@�Mj@�M@�M@�LY@�KI@�KI@�Jw@�I�@�I�@�G�@�F�@�D|@�D�@�D|@�C�@�A�@�@:@�@�@�@O@�?}@�?@�?>@�=�@�;%@�:i@�9�@�7�@�6z@�6&@�5i@�5~@�5i@�4�@�5~@�5i@�5�@�6;@�5�@�6&@�6�@�7L@�7L@�8	@�8	@�8\@�8�@�8�@�:*@�:�@�;:@�:~@�:�@�;:@�;:@�;�@�<K@�<�@�<`@�<�@�=\@�=@�=@�=\@�=�@�=�@�>@�>-@�=�@�>�@�>�@�?�@�?�@�@O@�@�@�A_@�At@�B@P��@P��@P�@P�0@P�,@P�5@P�9@P��@P��@P��@P�@P��@P�l@P��@P�@P��@P�@P�@P��@P�@P�H@Pߤ@P�@P�]@P۶@P�8@Pڐ@P�@@P��@Pؙ@P��@P�E@P�E@P�E@Pؙ@P�E@Pם@P�I@Pם@P��@P�I@P��@P֡@P��@P�I@P�I@P�I@P�I@P��@P��@P��@P֡@P�M@P��@P�M@P�M@P�M@P�M@Pզ@P��@PԪ@PԀ@PԀ@PԪ@PԀ@PԀ@PԀ@P�,@Pӄ@Pӄ@Pӄ@Pӄ@Pӄ@Pӄ@PӮ@Pӄ@Pӄ@P�1@P�1@P��@P��@P҉@Pҳ@Pҳ@P҉@P�5@P�5@P�5@P�5@P��@Pѷ@P��@P��@PΚ@P��@P�u@P��@P�.@P�2@PǏ@PǏ@P��@PǏ@PǏ@P�;@P�;@P�;@P�;@PƓ@P�?@Pŗ@P�C@P��@P��@P�L@P¤@P��@P�@P��@P�E@�K�@�K�@�K�@�L@�L�@�L�@�L�@�M+@�Ln@�L�@�M@�M�@�NQ@�N{@�N'@�NQ@�O�@�O"@�Oa@�O�@�R~@�Se@�S�@�T@�S@�R�@�R�@�S;@�T@�T"@�T@�S;@�R~@�TL@�R�@�R�@�R�@�SP@�S&@�S;@�TL@�T"@�T�@�T�@�S�@�T�@�V.@�U�@�V.@�U�@�V�@�V�@�V�@�W @�W*@�W @�W @�Wi@�Wi@�W�@�Wi@�Wi@�Wi@�Wi@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�X%@�X:@�Xd@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�YK@�Y6@�Y`@�YK@�Y6@�Y6@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�Y!@�X�@�V�@�Y6@�X�@�X�@�Xy@�X�@�X�@�X%@�W�@�W~@�W�@�W*@�W�@�W�@�W�@�Wi@�V�@�V�@�U�@�T�@�T�@�U2@�U�@�V@�V.@�U�@�T�@�T�@�T�@�T�@�T�@�T�@�T�@�Tv@�TL@�S�@�Sz@�Q�@�R�@�R�@�R�@�R�@�RT@�Q�@�Ri@�P�@�P�@�P�@�O�@�OL@�M�@�K�@�K^@�Jb@�JM@�I�@�H�@�F�@�Ex@�E�@�D|@�D|@�E$@�C�@�Bp@�A@�>�@�<@�9�@�9@�8�@�9@�8\@�7�@�7�@�7�@�82@�8\@�8\@�8�@�8�@�9@�9�@�9�@�: @�:�@�:�@�;:@�;�@�<u@�=G@�=2@�=2@�=G@�=@�=�@�>@�>�@�>�@�>�@�?)@�?)@�?>@�?h@�?�@�?�@�?�@�@%@�@%@�?�@�@%@�@�@�A�@�At@�At@�B�@�Bp@�A�@P�E@P��@P�s@P��@P��@P�V@P�@P�@P�>@P�@P��@P��@P��@P�@P�@P�t@P��@P�@P�@P�@P��@P��@P��@P��@P۶@Pپ@P�j@P�@P��@P��@P��@P�M@P�$@P��@P�$@P��@P��@P�|@P�(@P��@P��@P��@P��@P��@P��@P��@P�(@P�(@P��@P��@P��@PԀ@PԪ@P�@P�@P�,@P�V@P�V@P�,@P�[@P�@P҉@P҉@P҉@P҉@P҉@P҉@P��@P�9@P�@P�9@P�@P�c@Pэ@P�c@P�9@P�9@P�@P��@P��@Pл@P��@PБ@Pл@P��@P��@Pл@Pл@PБ@PБ@P�>@P�>@P�l@P͟@P�!@P̣@P�}@PȊ@P�@P�C@P�C@P��@P��@P��@P�m@Pŗ@Pŗ@P�m@Pŗ@P�@P��@P��@P�r@P�L@P�z@P�z@P�z@P�'@P�U@P�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           44444444443444444444444444444444444444444444444444444444444444444344444444444444444444444444444334443444444444444444443444444444444444444444444444444444443344444444444444434443444443344344344434444444443344444443444443343444434333433333343333433334333333433344333333333343343333333333333334333333333333433333344333333343333333433333343343334334333333333333333333333333333334333333343333334333433333333343333433333333333333333333434333344433333333433334333344333333333333344333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@� EG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�.�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�H@�%G�O�G�O�G�O�@�JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�:@�1(G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�7�G�O�G�O�G�O�@�:�G�O�G�O�G�O�G�O�G�O�@�.5@�2:G�O�G�O�@[��G�O�G�O�@�:�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@`!�@�?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�G�O�G�O�G�O�G�O�G�O�@nI�@��"G�O�@�?|G�O�G�O�G�O�G�O�@�CFG�O�@�@;@�DT@k��G�O�@�B@�B@�E�@�B@�B@�>�G�O�@g�@�C�@�F�@�C�G�O�@�B@�<�@�D{@�C�G�O�@�C�@��@�B�@�E�@�E5@�D'G�O�@�AJ@�C�@�BG�O�G�O�@�:,@�@�@�?�@�<�@�>p@�B�@�C�@�@�@��|@�=�G�O�@�B
@�?~G�O�@�=�@�?�@l4F@�<�@�=	@�<8@�>�@�:�@��@�:�@�:�@�;�@�:�@�>�@�:-G�O�@�<�@�=^@�;�@pY#@�;�@�@;@�<:@�?-@�<�@�?�@�?|@�@�G�O�@�N@�?�@�Av@�?B@�@;@�5�G�O�G�O�@�>l@�W�@�>�@�=@�Aw@�@9@�@>G�O�@�@9@�?/@�<6@��@���@�@;@�?�G�O�@�@�@�=[@�<�@�Av@�@N@h��G�O�@�@;@�@<G�O�@�A�@�B@�B�G�O�@�B�@�AaG�O�@�B�@�B\@��!@�B�@�B�@�C@�B@�BZ@�AJ@�@9@�?�@�<�@[x�@b�:@��F@�B�@�B�@�B�@�CB@�0�@�>+@�C�@�C�@�C�@�C�@�C�@�C~@�D*@�%G�O�@�C�@�C�@�B@�B]@�B]@�B^@�B�G�O�@�C@�D|@�C�@�C�@�B�@�B[G�O�@�A�@�A�@�?*G�O�@�B�@�A�@�@;@k��@�?�@�AJ@�A�@�A@�@>G�O�@�=�@�>�@�@�@�ALG�O�@�?)@�AJ@�A�@�A�@�AJ@���@�Av@�>�@�?�@�>�@g�@@�@;@�?*@�A@�B�@�@?@�@�@�B[@�C�@�B�G�O�@�ALG�O�@�C�@�B@�@�@�D*G�O�G�O�G�O�@�?~@�=[@�C�@�A�@�@�@�A�@�@R@�AFG�O�@�?@�?�@�?�@�>�G�O�@�?�@�?~@�?~@�?�G�O�G�O�@�>n@X��@�?~@�;>@�:.@�;>@�<4@�=�@�=�@�C�@�D(@�AL@�?�G�O�G�O�@�@@�I(@�B�@�@�@_�(@�E�@�F^@�B�@�F�@�E�@�H@�G�@�H@�G�@�G�@�F�@�F�@�Go@�Gm@�Go@�H@�H�@�H@�I%@�H�@�H�@�H�@�H�@�H{@�H�@�H�@�H�@�G�@�HT@�H.@�Gr@�G�@�I�@�H�@�H�@�HV@�GF@�G@�Gr@�Go@�F�@�G@�GF@�GI@�G�@�K�@�K�@�K�@�L@�L�@�L�@�L�@�M,@�Lm@�L�@�M@�M�@�NS@�N|@�N*@�NP@�O�@�O#@�Ob@�O�@�R~@�Sf@�S�@�T@�S@�R�@�R�@�S8@�T@�T"@�T@�S>@�R~@�TM@�R�@�R�@�R�@�SP@�S)@�S:@�TP@�T!@�T�@�T�@�S�@�T�@�V1@�U�@�V1@�U�@�V�@�V�@�V�@�V�@�W,@�W@�V�@�Wj@�Wj@�W�@�Wj@�Wn@�Wf@�Wi@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�X(@�X;@�Xd@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�YJ@�Y9@�Y_@�YJ@�Y:@�Y6@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�Y$@�X�@�V�@�Y9@�X�@�X�@�Xz@�X�@�X�@�X&@�W�@�W~@�W�@�W+@�W�@�W�@�W�@�Wo@�V�@�V�@�U�@�T�@�T�@�U4@�U�@�V@�V0@�U�@�T�@�T�@�T�@�T�@�T�@�T�@�T�@�Tx@�TL@�S�@�Sz@�Q�@�R�@�R�@�R�@�R�@�RR@�Q�@�Rj@�P�@�P�@�P�@�O�@�ON@�M�@�K�@�K`@�Je@�JM@�I�@�H�@�F�@�Ex@�E�@�D}@�D|@�E%@�C�@�Bn@�A@�>�@�<@�9�@�9@�8�@�9@�8[@�7�@�7�@�7�@�82@�8a@�8\@�8�@�8�@�9@�9�@�9�@�: @�:�@�:�@�;>@�;�@�<x@�=J@�=5@�=5@�=F@�=	@�=�@�>@�>�@�>�@�>�@�?&@�?,@�??@�?h@�?�@�?�@�@ @�@&@�@%@�?�@�@%@�@�@�A�@�Av@�Ay@�B�@�Bo@�A�@P�C@P��@P�u@P��@P��@P�Z@P�@P�@P�;@P�@P��@P��@P�@P�@P�@P�r@P��@P�@P�@P�@P��@P��@P��@P��@P۶@P��@P�k@P�"@P��@P��@P��@P�M@P� @P��@P�#@P��@P��@P�}@P�(@P��@P�@P��@P��@P��@P��@P��@P�&@P�(@P��@P��@P��@P�{@PԪ@P�@P� @P�+@P�U@P�U@P�*@P�]@P�@Pҍ@P҈@P҈@PҊ@P҆@PҊ@P��@P�;@P�@P�:@P�@P�f@Pэ@P�f@P�8@P�:@P�@P��@P��@Pи@P��@PЖ@Pк@P��@P��@Pн@Pн@PА@PЕ@P�>@P�>@P�n@P͠@P�@P̣@P˂@PȊ@P�
@P�B@P�@@P��@P��@P��@P�p@PŘ@PŚ@P�k@PŘ@P�@P��@P��@P�r@P�M@P�z@P�z@P�u@P�%@P�S@P�k@�K�@�K�@�K�@�L@�L�@�L�@�L�@�M,@�Lm@�L�@�M@�M�@�NS@�N|@�N*@�NP@�O�@�O#@�Ob@�O�@�R~@�Sf@�S�@�T@�S@�R�@�R�@�S8@�T@�T"@�T@�S>@�R~@�TM@�R�@�R�@�R�@�SP@�S)@�S:@�TP@�T!@�T�@�T�@�S�@�T�@�V1@�U�@�V1@�U�@�V�@�V�@�V�@�V�@�W,@�W@�V�@�Wj@�Wj@�W�@�Wj@�Wn@�Wf@�Wi@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�X(@�X;@�Xd@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�YJ@�Y9@�Y_@�YJ@�Y:@�Y6@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�X�@�Y$@�X�@�V�@�Y9@�X�@�X�@�Xz@�X�@�X�@�X&@�W�@�W~@�W�@�W+@�W�@�W�@�W�@�Wo@�V�@�V�@�U�@�T�@�T�@�U4@�U�@�V@�V0@�U�@�T�@�T�@�T�@�T�@�T�@�T�@�T�@�Tx@�TL@�S�@�Sz@�Q�@�R�@�R�@�R�@�R�@�RR@�Q�@�Rj@�P�@�P�@�P�@�O�@�ON@�M�@�K�@�K`@�Je@�JM@�I�@�H�@�F�@�Ex@�E�@�D}@�D|@�E%@�C�@�Bn@�A@�>�@�<@�9�@�9@�8�@�9@�8[@�7�@�7�@�7�@�82@�8a@�8\@�8�@�8�@�9@�9�@�9�@�: @�:�@�:�@�;>@�;�@�<x@�=J@�=5@�=5@�=F@�=	@�=�@�>@�>�@�>�@�>�@�?&@�?,@�??@�?h@�?�@�?�@�@ @�@&@�@%@�?�@�@%@�@�@�A�@�Av@�Ay@�B�@�Bo@�A�@P�C@P��@P�u@P��@P��@P�Z@P�@P�@P�;@P�@P��@P��@P�@P�@P�@P�r@P��@P�@P�@P�@P��@P��@P��@P��@P۶@P��@P�k@P�"@P��@P��@P��@P�M@P� @P��@P�#@P��@P��@P�}@P�(@P��@P�@P��@P��@P��@P��@P��@P�&@P�(@P��@P��@P��@P�{@PԪ@P�@P� @P�+@P�U@P�U@P�*@P�]@P�@Pҍ@P҈@P҈@PҊ@P҆@PҊ@P��@P�;@P�@P�:@P�@P�f@Pэ@P�f@P�8@P�:@P�@P��@P��@Pи@P��@PЖ@Pк@P��@P��@Pн@Pн@PА@PЕ@P�>@P�>@P�n@P͠@P�@P̣@P˂@PȊ@P�
@P�B@P�@@P��@P��@P��@P�p@PŘ@PŚ@P�k@PŘ@P�@P��@P��@P�r@P�M@P�z@P�z@P�u@P�%@P�S@P�kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           44444444443444444444444444444444444444444444444444444444444444444344444444444444444444444444444334443444444444444444443444444444444444444444444444444444443344444444444444434443444443344344344434444444443344444443444443343444434333433333343333433334333333433344333333333343343333333333333334333333333333433333344333333343333333433333343343334334333333333333333333333333333334333333343333334333433333333343333433333333333333333333434333344433333333433334333344333333333333344333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�B*9�B9�B/9�Bw9�B�9�C9�B�9�C�9�B�9�B�9�C_9�DF9�D�9�D�9�Dn9�D�9�E�9�EQ9�E�9�E�9�Hb9�I59�I�9�I�9�H�9�H�9�H�9�I9�I�9�I�9�I�9�I9�Hb9�J9�H�9�H�9�H�9�I!9�H�9�I9�J9�I�9�J�9�J@9�I9�J�9�K�9�K�9�K�9�K�9�L69�L29�Ll9�L|9�L�9�L�9�L~9�L�9�L�9�M9�L�9�L�9�L�9�L�9�M-9�M9�M9�M9�M-9�M-9�Md9�M)9�M�9�M�9�M�9�M�9�N9�N99�M�9�N89�NL9�NK9�N�9�N�9�N�9�N�9�N�9�N�9�NN9�NN9�N;9�NK9�N89�N89�NL9�NN9�Nt9�NN9�Ln9�N�9�N9�N9�M�9�N:9�N9�M�9�M)9�L�9�M9�L�9�MO9�M?9�MR9�L�9�Lk9�LG9�KN9�JU9�Jg9�J�9�K=9�K�9�K�9�KL9�J�9�J�9�J�9�JW9�Jj9�Jf9�Jl9�J09�J9�I�9�IH9�G|9�Hw9�H�9�H�9�Hv9�H99�G�9�HO9�F�9�F�9�F�9�E�9�Ex9�D9�BS9�A�9�@�9�@�9�@99�?�9�=�9�<}9�<�9�;�9�;�9�<19�:�9�9�9�8u9�629�3�9�1�9�19�0�9�19�0�9�0'9�/�9�0"9�0^9�0�9�0�9�0�9�0�9�19�1�9�1�9�29�2�9�2�9�3&9�3�9�4E9�59�4�9�4�9�59�4�9�5b9�5�9�6"9�6�9�6�9�6�9�6�9�6�9�6�9�7B9�7d9�7~9�7�9�7�9�7y9�7�9�89�8�9�8�9�8�9�:*9�9�9�99>Ӎ9>��9>��9>Ο9>χ9>��9>͓9>��9>�79>�9>��9>��9>��9>�I9>��9>ɬ9>�9>�9>�29>��9>��9>�9>�%9>�^9>�89>�m9>� 9>�	9>��9>��9>��9>�G9>�9>��9>� 9>��9>��9>��9>�;9>�9>�9>��9>��9>��9>��9>�9>�99>�;9>�9>��9>��9>��9>��9>�49>�-9>�T9>�{9>�{9>�S9>��9>�J9>��9>��9>��9>��9>��9>��9>�?9>��9>�|9>��9>�|9>��9>��9>��9>��9>��9>�~9>�X9>�Z9>�.9>�U9>�9>�09>�T9>�X9>�29>�29>�	9>�9>��9>��9>� 9>�[9>��9>�s9>�l9>��9>�A9>��9>��9>�P9>�+9>�+9>��9>�9>�9>��9>�9>��9>�l9>�B9>��9>��9>�,9>�,9>�(9>��9>�9>��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�By�B{�B|�B~�B� B�B�DB�oB��B�?BǮB��B��B��B�B�BB�B�B�yB�sB�yB�B��B��B��B��B��B��BB	7B1BBB	7B�B9XBG�BN�BW
BffB�B��B�B�?BĜB��B�BB�ZB�fB�B��B��B�B�5B��BȴBȴBȴB��B��BɺB��B�}B��B��B��B�
B��B��B��BÖB�9B��B��B�uB�%Bz�Bv�Bv�Bs�Bn�BbNBJ�B(�B�B�BoBJB%B��B�BȴB~�B[#BO�BA�B<jB#�B
��B
�^B
��B
�B
��B
��B
��B
� B
^5B
R�B
2-B

=B	��B	�B	�;B	�/B	�)B	�B	ȴB	�B	�PB	~�B	q�B	iyB	bNB	bNB	_;B	YB	VB	W
B	J�B	6FB	,B	!�B	�B	�B	{B	\B	JB	DB	JB		7B	  B�B�ZB�;B�B��B��B��B��BɺBǮBŢB��B�wB�dB�LB�-B�B��B��B��B�bB�DB�B� Bw�Br�Bp�Bn�Bl�BjBiyBgmBe`BcTB`BB]/BZBXBT�BS�BR�BQ�BP�BN�BL�BJ�BH�BG�BF�BD�BA�B=qB:^B7LB6FB5?B49B49B33B33B33B33B2-B1'B1'B0!B0!B0!B/B.B.B-B-B)�B+B+B+B+B)�B(�B)�B)�B+B+B+B+B)�B)�B)�B)�B(�B(�B'�B'�B&�B'�B&�B%�B%�B%�B%�B$�B%�B)�B)�B)�B,B-B1'B7LB9XB=qBB�BE�BF�BF�BH�BJ�BM�BO�BQ�BT�BXBZBcTBffBiyBiyBiyBn�Bq�Bt�B� B�B�B�B�%B�%B�1B�=B�DB�JB�DB�=B�1B�7B�=B�=B�7B�7B�1B�DB�JB�PB�bB�bB�bB�oB�{B��B��B��B��B��B��B��B�B�9B�FB�jB�wB��BǮB��B�B�)B�5B�5B�;B�BB�HB�TB�ZB�ZB�ZB�fB�mB�sB�B�B�B�B�B�B��B��B	B	%B	JB	PB	\B	bB	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	(�B	,B	1'B	2-B	33B	49B	8RB	>wB	A�B	B�B	M�B	Q�B	S�B	T�B	T�B	VB	W
B	YB	ZB	[#B	\)B	]/B	^5B	`BB	bNB	cTB	dZB	e`B	ffB	gmB	gmB	gmB	hsB	iyB	n�B	u�B	{�B	{�B	|�B	|�B	|�B	}�B	}�B	~�B	�B	�B	�B	�1B	�7B	�DB	�VB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�'B	�-B	�9B	�?B	�?B	�FB	�FB	�FB	�FB	�RB	�XB	�^B	�}B	�}B	�}B	��B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�)B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�lB
�B
SB
mB
)yB
.}B
7�B
@OB
G�B
KxB
O�B
RTB
Z7B
_!B
b�B
eB
i�B
nIB
rB
tnG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�%�>�v>�_F>�QP>��>�b ?-8�A �>�I?c��Bi�?��m>r�>�`�>���>��>��>���>��?R��?c�>�*>��c?4�d?��>B��>H<)>Ob�>I�k>N�s>Je�>y }>�b�>�t�>�|>��G?5��>�pX?3>Mo>a�>uc�>���>��M>���>�bg?"&@��>Ys�>���>ض>6F�>7�)>;�j>S<>a1�>x8�>�d>��*>��>�=�?'>���>�Q�?~�tBڍ>�p>�\�?'�ZA/Gc>�t>��>��>�'?T��@.�@���>��!>�:>��>�_�>���?�|?"�@��>ޚN>w��>�H�>~�>�;�>��W>��s>�<�>��F?��B[�B`�?JI?�?�UPB{
?hK?x�?_�j@�4>5�>I�>>H�>bJ'>]	�>���>��f>���>��>�֑>�>G>��X??�aB�Q>�0�>�2�>�>�ޘ>�г>��>���>Ǖ�>�m�?W?B�?��[@���>�a�>���>�qm>�Ui?E�>b�n>n��>���>��&>��l>�>�>��K>��?��?O�o@��s>��??(��A!65@�w?7��?�)RBy�Bv�>��>��h?.�>�b�>�Fo>�&[>��?4�@�jfA��8>�e�@2�>�7*?& @�)�BsLAuB?�?`�wBy>�O�>�}N?%ҿA	��?��Bq�B��?�?��@A�	�?~�?��bB��>�q�?��?��B@�@Ž�?ɂwAs>�s?7V@�,I>ѡ�?U?_��A��TBt�@�t�>�,�?
�?�;�@0�EA?�@�kB�A�81@�/�?�,>��?<F�A��vB��?aȆB|�@��A6!CA��?zǨA�N@�WMBw,B�ZA���?��JBw@By�Bx�Bt�BwSBwdA��TA� Bw�Bv�Bx�@�/\B��Bq BvfBuzA�'WBzxB	:�Bv�B{KBw	B^@ƃ�Bu�ByUBy�A,qi@�Y�B��B|�Bw�By�Bw�Bz@BwDB��B��B�BA��BwHBx�A5��BvmB��A�"Bv�B�3Bu�Bw\ByEB�MBw�B~}Bw�Bw�Bu�B�Ap��BxBxvB��A���By�Bw�BuBz�BwB�ZBu�Bx�@�4A���By=Bv�Bu�Bw,A�:�@^hAy��BvYB��Bu�Bu�B{eBveBw�@���BzHBx�B�A�L�A׽~By�Bv�@��qBwBv)B{�Bv�Bx�A�\^?q�#B{B�Ak��BzBw@Bv{A:Bw�B�M?�nBx�BtxB
��Bv�Bv$Bv�By�Bu?Bu�BveBvBt�A���A���B	3ZBu�B}\Bw�B{qB
�fB��BvIBv�Bv�BvTBu�Bt�B��A�g�A&�BwTBv�Bt�Bu7Bu7BxKB��@3�Bu�Bw%BxBv�Bw�Bv�?��"Bv5Bv�Bw�?��&Bw�B��BvmA��<Bv�B��BwBw�Bw�A�E(Bx)BvlBv�Bw\?��Bx�BzxB�LBv@BwLB�Bv�B�?Bv�Bu�A���Bw,Bw�Bw�BwBB�BBy�ByBw B{@�:�By�@�L�Bw�BvqBw�By&@&�A8�AkuBx�By=Bv9ByIByBv8Bu�B|�@��fBx�Bv�Bw�B��@a~gBxvBx�Bt�Bz�?��!@�AxB}PA���BxBx�BytBx�Bx3BveBx�Bu�Bw�BxB��? ��@E#�B�*B��BwBz�A���B}�Bx�Bx	BwbBxcBzQBv"Bw5Bt�Bv�Bv�Bv�Bv|Bw;Bx
BvEBwcBv5Bv]Bu�Bv	Bv	Bv|Bu�Bu�BvtBvlBuBunBvBv#Bu�Bv{Bu�BuBvBtOBu�Bv�Bu�BvBv[Bu�Bv�Bv�Bv|Bv�Bv�Bw�Bu�Bw Bu�BwBv�BvmBv�Bu�Bv�BwtBwBw[Bw�Bw'Bw�BxkBw�BxqBx�BxDBw�Bw�Bw�Bx|Bx|BxYBw�Bx\BxBw|Bw�Bw�Bw�Bx�BxBxjBxbBxQBx�Bx�ByLBytBy�Bx�Bx�B{Bz�Bz2Bz�BzyB{�B{|B{(Bz�B|#B{gBz�B|BzBzwB{oB{zB|�B~~B}�B~�B}�B|�B{}B{�B~�B|B|�B{MB{�B|cB|�B{B{Bz�B|Bz�B{7Bz�B~�B}DB�B��B�BpB�B~�B�B~mB~�B~�B�_B�1B�uB�B�ZB��B��B� B��B��B��B��B�$B��B�iB�;B��B��B��B�@B��B�B��B��B�5B��B�B��B�DB�kB�PB�@B��B�^B�B��B��B��B�xB��B�BB�2B�B�B�RB��B��B�WB�B�;B��B�]B�B��B�0B��B��B��B�	B��B�B��B��B�B��B��B�\B��B��B��B�gB��B��B�-B�B��B��B�B�zBÑB��BƃBųB��B��B��B�AB�`B��B��BɌB��B��B�B��B��B�rB�BʷB�vB�B�^B� B��B�)B�4B��B�hB�;B�PB��B��B�_B��BΝB�eB	�B	��B	�B	�cB	��B	�rB	�B	�B	�hB	�[B	�B	�B	�B	�B	�~B	�=B	�-B	�B	�B	��B	�B	��B	�B	�B	� B	��B	�B	�,B	��B	�B	��B	�FB	�IB	�<B	�\B	�B	�B	�AB	�B	��B	�B	��B	�B	�B	��B	��B	��B	�B	�\B	�_B	�RB	��B	�B	�dB	�B	�zB	�mB	�`B	��B	�RB	�B	��B	��B	��B	�B	�B	�B	�VB	��B	��B	�B	�B	�B	�qB	�B	�IB	�MB	�B	��B	�B	�B	�EB	�ZB	�MB	�B	��B	��B	�B	�B	�IB	�B	�vB	��B	��B	�IB	��B	��B	�B	��B	�B	�B	��B	�kB	�^B	�B	�B	��B	��B	�fB	�B	�B	�LB	�B	�?B	�B	�?B	�B	�?B	��B	�By�By�BzmBy#BzTBy�BzDBz�Bz!ByeBy�By�Bz'Bz=By�BzBz�Bz�BzByvB{JB|B{�B{�B{�B{nB{qB{�B|kB|nB{�B{�Bz�B{�B{B{B{B{_B{1B{<B|+B{�B|�B|KB{}B|�B}�B}fB}�B|�B}�B~�B}RB}UB~:B}EB~B}�BBBBB~�B~"B~B%B�B�B�GB�RB�JB�B��B�B��B�#B~�B�B�uB�4B�dB~�B~�B~NB�BB�B�B~�B�B��B��B��B��B��B�B��B�B��B��B�B�B�3B��B�,B�B�TB�
B��B��B�3B�@B�%B�iB�}B�B��B�B��B��B��B��B�vB��B�#B��B�/B��B�aB�4B�dB�\B�B�sB��B��B�dB��B�AB�LB�B��B�5B�B��B��B�B��B�4B��B��B��B�}B�aB�=B�B��B�8B��B�YB��B�JB�hB�rB��B�B��B�B��B��B��B�B�B��B�{B�;B�YB�B�%B�B��B�B�^B�|B�B�HB�AB�sB�B��B˪BˡBˬB�kB�4B�@B��B��B�)B�"B�RB�TB�sB̷B��BͧB��B��B͇B��B�B�\B�&B�B�4BκB�B	��B	�iB	�=B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�_B	�/B	��B	�B	�B	�QB	�B	�RB	��B	�LB	�B	�:B	� B	�B	�XB	�B	�B	�wB	�KB	��B	��B	�B	�B	�eB	�XB	��B	��B	�{B	�nB	�BB	�5B	�B	�B	�B	�0B	�B	��B	�B	��B	�iB	�{B	��B	��B	��B	��B	��B	��B	�B	��B	�mB	�`B	�SB	�FB	�,B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�rB	�FB	�+B	��B	�B	�B	��B	��B	��B	��B	�B	�mB	�SB	�B	�B	�WB	�B	��B	�EB	�dB	�NB	��B	��B	��B	�QB	�B	�B	��B	��B	��B	�B	�B	�7B	�B	��B	�B	��B	�B	� B	�B	�B	�$B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444443444444444444444444444444444444444444444444444444444444344444444444444444444444444444334443444444444444444443444444444444444444444444444444444443344444444444444434443444443344344344434444444443344444443444443343444434333433333343333433334333333433344333333333343343333333333333334333333333333433333344333333343333333433333343343334334333333333333333333333333333334333333343333334333433333333343333433333333333333333333434333344433333333433334333344333333333333344333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   By�By�B{�B|�BB�	B�B�MB�yB��B�HBǺB��B��B��B� B�OB�B�B�B�zB�B�B��B��B��B��B��B�BB	>B<BB%B	@B�B9_BG�BN�BWBfqB�B��B�B�HBħB�B�KB�gB�pB�B�B��B�B�@B��B��BȿB��B��B��B��B��B��B��B��B�B�B��B�B��BâB�CB��B��B�B�1Bz�Bv�Bv�Bs�Bn�BbZBJ�B)B�B�BxBVB.B��B�BȾBB[/BO�BA�B<uB#�B
��B
�mB
��B
�'B
��B
��B
��B
�B
^?B
R�B
2:B

JB	��B	�B	�GB	�;B	�7B	�&B	��B	�B	�\B	B	q�B	i�B	b\B	b[B	_GB	Y#B	VB	WB	J�B	6QB	,B	!�B	�B	�B	�B	iB	VB	QB	XB		BB	 B�B�eB�FB�B� B��B��B��B��BǼBŰB��B��B�sB�YB�<B�#B��B��B��B�oB�RB�.B�Bw�Br�Bp�Bn�Bl�Bj�Bi�Bg|BenBcaB`QB]?BZ-BXBUBTBSBQ�BP�BN�BL�BJ�BH�BG�BF�BD�BA�B=�B:kB7XB6TB5MB4FB4GB3AB3CB3AB3CB29B15B16B01B0-B0.B/+B. B.$B-B-B*
B+B+B+B+B*B)B*B*B+B+B+B+B*
B*B*	B*B)B)B'�B'�B&�B( B&�B%�B%�B%�B%�B$�B%�B*B*	B*B,B-B18B7ZB9gB=BB�BE�BF�BF�BH�BJ�BM�BO�BQ�BUBXBZ-BceBfvBi�Bi�Bi�Bn�Bq�Bt�B�B�(B�.B�0B�5B�6B�>B�NB�VB�ZB�VB�KB�AB�HB�MB�NB�FB�EB�?B�QB�XB�`B�qB�rB�rB�B��B��B��B��B��B��B��B��B�'B�JB�SB�yB��B��BǼB��B�*B�:B�FB�EB�KB�TB�XB�dB�iB�kB�lB�vB�}B�B�B��B�B�B�B�B��B��B	B	5B	YB	aB	lB	qB	~B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	)B	,B	17B	2<B	3CB	4JB	8cB	>�B	A�B	B�B	M�B	Q�B	T	B	UB	UB	VB	WB	Y(B	Z0B	[2B	\;B	]=B	^EB	`QB	b_B	cgB	diB	epB	fwB	gB	gzB	g}B	h�B	i�B	n�B	u�B	{�B	{�B	|�B	|�B	|�B	~B	~B	
B	�B	�(B	�,B	�@B	�GB	�UB	�gB	�gB	�rB	�zB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�7B	�8B	�8B	�=B	�HB	�OB	�OB	�VB	�TB	�UB	�UB	�`B	�hB	�mB	��B	��B	��B	��B	ŰB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�5B	�1B	�:B	�BB	�KB	�JB	�WB	�^B	�fB	�kB	�kB	�pB	�pB	�pB	�pB	�pB	�wB	�vB	�vB	�vB	�tB	�{B	�B	�B	�B	�B	��B	��B	��B	�G�O�B	�/B	�~B
�B
cB
}B
)�B
.�B
7�B
@`B
G�B
K�B
O�B
RcB
ZHB
_0B
b�B
e#B
i�B
nZB
r#B
t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bi�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BڗG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B[�B`�G�O�G�O�G�O�B{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�Bv�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BsTG�O�G�O�G�O�ByG�O�G�O�G�O�G�O�G�O�Bq�B��G�O�G�O�A�	�G�O�G�O�B��G�O�G�O�G�O�B@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��dBt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�A�ׇB��G�O�B|�G�O�G�O�G�O�G�O�A�NG�O�Bw4B�eA��G�O�BwGBy�Bx�BuBw]BwmG�O�A�Bw�Bv�Bx�G�O�B��Bq)BvoBu�G�O�Bz�B	; Bv�B{UBwBgG�O�Bu�By`By�G�O�G�O�B��B|�Bw�By�Bw�BzJBwOB��B��B�MG�O�BwTBx�G�O�BvvB��A�0Bv�B�=Bu�BwdByPB�XBw�B~�Bw�Bw�Bu�B�G�O�Bx Bx�B��A���BzBw�Bu#Bz�BwB�fBu�Bx�G�O�A���ByEBv�Bu�Bw4A�:�G�O�G�O�BvbB��Bu�Bu�B{pBvnBw�G�O�BzRBx�B�A�L�A׽�By�Bv�G�O�Bw Bv1B|Bv�Bx�A�\gG�O�B{B�G�O�Bz"BwGBv�G�O�Bw�B�XG�O�Bx�Bt�B
��Bv�Bv-Bv�By�BuHBu�BvnBv"BuA���A���B	3fBu�B}gBw�B{{B
�pB��BvTBv�Bv�Bv^Bu�Bt�B��A�g�G�O�Bw]Bv�Bt�BuBBuBBxVB��G�O�Bu�Bw/Bx$Bv�Bw�Bv�G�O�Bv?Bv�Bw�G�O�Bw�B��BvxA��KBv�B��BwBw�Bw�G�O�Bx4BvtBv�BwfG�O�Bx�Bz�B�UBvJBwVB�Bv�B�IBv�Bu�A���Bw4Bw�Bw�BwKB�NBy�By%Bw	B{G�O�By�G�O�Bw�Bv|Bw�By.G�O�G�O�G�O�Bx�ByEBvCBySByBvBBu�B|�G�O�Bx�Bv�Bw�B��G�O�Bx�Bx�Bt�Bz�G�O�G�O�B}\A���BxBx�ByBx�Bx=BvoBx�Bu�Bw�Bx$B��G�O�G�O�B�4B��Bw'Bz�A���B}�Bx�BxBwkBxlBz\Bv,Bw?Bt�Bv�Bv�Bv�Bv�BwDBxBvNBwnBv=BvfBu�BvBvBv�Bu�Bu�Bv|BvvBu BuvBvBv/Bu�Bv�Bu�Bu
BvBtXBu�Bv�BvBvBvgBu�Bv�Bv�By�By�BzyBy,Bz^By�BzMBz�Bz)BymBy�By�Bz0BzHBy�BzBz�Bz�Bz)By�B{UB|B{�B{�B{�B{yB{{B{�B|sB|wB{�B{�Bz�B{�B{B{!B{B{iB{;B{EB|7B{�B|�B|VB{�B|�B}�B}nB}�B|�B}�B~�B}\B}\B~DB}NB~B}�BBMBBB~+B~%B1B�B�B�OB�[B�TB�B� B�B��B�,B~�B�B�}B�>B�oB~�B~�B~XB�B B�B�B~�B�(B��B��B��B��B��B�B�B�!B��B��B�B�)B�=B��B�8B�B�_B�B��B��B�>B�HB�/B�sB��B�)B��B�B��B�B��B��B�~B��B�-B��B�8B��B�lB�?B�mB�iB�B�}B��B��B�mB�B�KB�SB�(B��B�@B�B��B��B� B��B�<B��B��B��B��B�jB�FB�"B��B�AB��B�bB��B�RB�qB�xB��B�B��B�B��B��B��B�B�B��B��B�DB�cB�"B�.B�B��B�$B�hBǆB�B�RB�KB�}B�!B��B˶B˫B˶B�uB�<B�JB��B�B�4B�+B�^B�aB�|B��B��BʹB�B��B͏B��B�B�eB�1B�+B�<B��B�B	��B	�yB	�MB	�B	�B	�B	�)B	�B	��B	��B	�B	�B	�nB	�?B	��B	�B	�&B	�_B	�-B	�aB	��B	�\B	�B	�JB	�B	�B	�iB	�B	�B	�B	�\B	��B	��B	�B	��B	�vB	�iB	� B	��B	�B	�~B	�SB	�DB	�*B	�B	�0B	�>B	�&B	��B	��B	��B	�vB	�B	�B	��B	��B	�
B	��B	��B	�0B	��B	�}B	�nB	�cB	�VB	�:B	�.B	��B	� B	��B	�B	��B	��B	�B	��B	��B	��B	�B	�VB	�=B	�B	�B	��B	��B	��B	��B	��B	��B	�|B	�dB	�B	�B	�fB	�B	��B	�UB	�tB	�]B	�B	� B	��B	�`B	�'B	�B	��B	��B	��B	�B	�B	�HB	�B	��B	�B	��B	�,B	�.B	�B	��B	�2B	�By�By�BzyBy,Bz^By�BzMBz�Bz)BymBy�By�Bz0BzHBy�BzBz�Bz�Bz)By�B{UB|B{�B{�B{�B{yB{{B{�B|sB|wB{�B{�Bz�B{�B{B{!B{B{iB{;B{EB|7B{�B|�B|VB{�B|�B}�B}nB}�B|�B}�B~�B}\B}\B~DB}NB~B}�BBMBBB~+B~%B1B�B�B�OB�[B�TB�B� B�B��B�,B~�B�B�}B�>B�oB~�B~�B~XB�B B�B�B~�B�(B��B��B��B��B��B�B�B�!B��B��B�B�)B�=B��B�8B�B�_B�B��B��B�>B�HB�/B�sB��B�)B��B�B��B�B��B��B�~B��B�-B��B�8B��B�lB�?B�mB�iB�B�}B��B��B�mB�B�KB�SB�(B��B�@B�B��B��B� B��B�<B��B��B��B��B�jB�FB�"B��B�AB��B�bB��B�RB�qB�xB��B�B��B�B��B��B��B�B�B��B��B�DB�cB�"B�.B�B��B�$B�hBǆB�B�RB�KB�}B�!B��B˶B˫B˶B�uB�<B�JB��B�B�4B�+B�^B�aB�|B��B��BʹB�B��B͏B��B�B�eB�1B�+B�<B��B�B	��B	�yB	�MB	�B	�B	�B	�)B	�B	��B	��B	�B	�B	�nB	�?B	��B	�B	�&B	�_B	�-B	�aB	��B	�\B	�B	�JB	�B	�B	�iB	�B	�B	�B	�\B	��B	��B	�B	��B	�vB	�iB	� B	��B	�B	�~B	�SB	�DB	�*B	�B	�0B	�>B	�&B	��B	��B	��B	�vB	�B	�B	��B	��B	�
B	��B	��B	�0B	��B	�}B	�nB	�cB	�VB	�:B	�.B	��B	� B	��B	�B	��B	��B	�B	��B	��B	��B	�B	�VB	�=B	�B	�B	��B	��B	��B	��B	��B	��B	�|B	�dB	�B	�B	�fB	�B	��B	�UB	�tB	�]B	�B	� B	��B	�`B	�'B	�B	��B	��B	��B	�B	�B	�HB	�B	��B	�B	��B	�,B	�.B	�B	��B	�2B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444443444444444444444444444444444444444444444444444444444444344444444444444444444444444444334443444444444444444443444444444444444444444444444444444443344444444444444434443444443344344344434444444443344444443444443343444434333433333343333433334333333433344333333333343343333333333333334333333333333433333344333333343333333433333343343334334333333333333333333333333333334333333343333334333433333333343333433333333333333333333434333344433333333433334333344333333333333344333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281454402020082814544020200828145440202008281454402020082814544020200828145440202008281454402020082814544020200828145440202008281454402020082814544020200828145440AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730272019021417302720190214173027    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730272019021417302720190214173027  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730272019021417302720190214173027  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281454402020082814544020200828145440  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                