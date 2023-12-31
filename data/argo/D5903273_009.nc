CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  Y   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:32Z creation      
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
resolution        =���   axis      Z        (,  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  m<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (,  wH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (,  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (,  Ѭ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, 6   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (, ^H   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �t   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (, ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (, ¸   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (,    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 EH   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, OT   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � w�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   x@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �dArgo profile    3.1 1.2 19500101000000  20190219181632  20200831164707  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               	   	   	AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @Ք�~K�@Ք�~K�@Ք�~K�111 @Ք����`@Ք����`@Ք����`@7	�^5?}@7	�^5?}@7	�^5?}�c�\(��c�\(��c�\(�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    	   	   	ADA BDA  DA BDA @333@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�D��D�F�D���D��3D��D�@RD���D�ؤD� D�QHD��)Dǲ�D��D�3�D�~�D�3D��D�G\D�|{D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�ͽ��;L�;L��    =���    ���;L�;L�ͽ��ͽ��ͽ��ͽ���=��ͽ��ͽ���    >L��=��;L��=��ͽ��ͽ��;L��        �L��    =���=��ͽ��ͽ��ͽ��;L�ͽ���    ���ͽ��ͽ���=��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L��    ���ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ���        ���ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ���=���=��ͽ��;L�ͽ���=���=��ͽ��ͽ��;L�ͽ���    �L�ͽ���    ���ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ���    �L�;L�ͽ��ͽ��ͽ��;L�ͽ���    >���    ���ͽ��ͽ���=��;L�;L�ͽ���    ���ͽ��ͽ��ͽ���=��ͽ��;L�;L��=��ͽ��ͽ���    >L��    ���;L�;L�ͽ���=���    ���ͽ��ͽ��;L�ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ���=���=��ͽ��ͽ��ͽ���=��ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ���    ���ͽ��ͽ���    ���ͽ���            ���ͽ���    =���    ����    ���ͽ���    =���=���>L��    ����    ����    >L��=��ͽ��ͽ���    ����            �L�ͽ��ͽ���=��ͽ���=���>L��>L��>L��=���    ���ͽ���    =���    =���=���=���>L��>���>���>L��>���=���    =���=���>L��    =���=���=���=���    >���=���>L��>L��>L��=���    =���    =���>L��=���=���=���>L��=���    =���=���=���    >L��    =���>L��=���=���=���>L��=���    =���>���=���        =���=���=���    =���=���    >L��=���=���=���=���>���=���    >L��>L��=���=���    =���=���=���>L��>L��    =���=���=���=���>L��>L��=���=���=���>L��=���=���=���=���=���=���    =���    ����>L��=���>L��>L��=���    =���=���=���=���=���=���=���=���=���=���=���=���>L��>L��=���=���        >L��>L��>L��>L��=���        =���=���=���>L��>L��    =���=���=���=���>L��>L��=���    =���            =���=���=���=���    =���=���>L��=���=���    =���        =���=���=���>L��    >L��>L��>L��>L��>L��>L��    =���>L��>L��=���=���=���>L��=���=��ͽ���    =���=���=���>���=���    =���=���    ����=���>L��=���=���>L��>L��=���=���=���=���>L��=���    =���=���=���=���    =���>���>L��=���>L��>L��    =���=���>L��=���=���=���=���=���=���        >L��=���    =���>L��=���=���=���>L��=���>L��=���    =���=���=���>L��>L��=���=���        =���>L��=���=���=���    =���=���>L��=���    =���=���=���=���=���=���=���    ����>L��>L��=���=���>���>���>���?   ?   ?   ?333?333?L��?fff?fff?�  ?�  ?���?�ff?�ff?�33?�  ?���?���?ٙ�?�33?�33@   @ff@ff@��@33@��@   @&ff@,��@9��@@  @L��@S33@Y��@`  @l��@s33@y��@�  @�ff@���@���@�  @�33@���@���@�  @�33@���@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@ٙ�@���@�33@�ff@陚@���@�  @�33@�ff@���@���A   A33A��AffA  A	��A33AffAffA  A33A��AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAI��AK33AL��ANffAP  AS33AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��AfffAi��Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���Ař�A�33A�  A���Aə�A�ffA�33A�  A͙�A�ffA�33A�  Aљ�A�ffA�33A�  A���Aՙ�A�33A�  A���Aٙ�A�ffA�33A���Dq�Dq&fDq,�Dq33Dq9�DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` Drl�Drs3Dry�Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr� Dr�fDr�3Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds@ DsFfDsL�DsS3Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3DtٚDt�fDt��Dt�3Du  Duf@&ff@,��@9��@@  @L��@S33@Y��@`  @l��@s33@y��@�  @�ff@���@���@�  @�33@���@���@�  @�33@���@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@ٙ�@���@�33@�ff@陚@���@�  @�33@�ff@���@���A   A33A��AffA  A	��A33AffAffA  A33A��AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAI��AK33AL��ANffAP  AS33AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��AfffAi��Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���Ař�A�33A�  A���Aə�A�ffA�33A�  A͙�A�ffA�33A�  Aљ�A�ffA�33A�  A���Aՙ�A�33A�  A���Aٙ�A�ffA�33A���Dq�Dq&fDq,�Dq33Dq9�DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` Drl�Drs3Dry�Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr� Dr�fDr�3Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds@ DsFfDsL�DsS3Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3DtٚDt�fDt��Dt�3Du  DufG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @:=p@��R@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA�A��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�B�B�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�pD
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq �Dq�
Dr
Dr�
Ds
Ds�
Dt
Dt�
Dt�pDy�)D�D�J=D��D�ƸD�fD�C�D��)D��)D��D�T�D���DǶD��D�7\Dڂ=DྸD�RD�J�D� D��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���Q�<#�	��Q콸Q�=�G�>W
>=�G�<#�	��Q콸Q�<#�	<#�	<#�	<#�	>W
><#�	<#�	=�G�>��R>W
>��Q�>W
><#�	<#�	��Q�=�G�=�G���Q�=�G�>W
>>W
><#�	<#�	<#�	��Q�<#�	=�G�<#�	<#�	<#�	>W
><#�	��Q�<#�	<#�	<#�	<#�	<#�	<#�	��Q�=�G�<#�	<#�	<#�	<#�	<#�	��Q�<#�	<#�	=�G�=�G�<#�	<#�	<#�	<#�	��Q�<#�	<#�	<#�	>W
>>W
><#�	��Q�<#�	>W
>>W
><#�	<#�	��Q�<#�	=�G���Q�<#�	=�G�<#�	<#�	<#�	��Q�<#�	<#�	<#�	<#�	<#�	=�G���Q콸Q�<#�	<#�	<#�	��Q�<#�	=�G�>��=�G�<#�	<#�	<#�	>W
>��Q콸Q�<#�	=�G�<#�	<#�	<#�	<#�	>W
><#�	��Q콸Q�>W
><#�	<#�	=�G�>��R=�G�<#�	��Q콸Q�<#�	>W
>=�G�<#�	<#�	<#�	��Q�<#�	=�G�<#�	<#�	<#�	<#�	<#�	<#�	>W
>>W
><#�	<#�	<#�	>W
><#�	<#�	��Q콸Q�<#�	<#�	<#�	<#�	��Q�<#�	<#�	<#�	<#�	<#�	��Q�<#�	=�G�<#�	<#�	<#�	=�G�<#�	<#�	=�G�=�G�=�G�<#�	<#�	=�G�>W
>=�G�<#�	=�G�<#�	<#�	=�G�>W
>>W
>>��R=�G�<#�	=�G�<#�	=�G�>��R>W
><#�	<#�	=�G�<#�	=�G�=�G�=�G���Q�<#�	<#�	>W
><#�	>W
>>��R>��R>��R>W
>=�G�<#�	<#�	=�G�>W
>=�G�>W
>>W
>>W
>>��R>��>��>��R>��>W
>=�G�>W
>>W
>>��R=�G�>W
>>W
>>W
>>W
>=�G�>��>W
>>��R>��R>��R>W
>=�G�>W
>=�G�>W
>>��R>W
>>W
>>W
>>��R>W
>=�G�>W
>>W
>>W
>=�G�>��R=�G�>W
>>��R>W
>>W
>>W
>>��R>W
>=�G�>W
>>��>W
>=�G�=�G�>W
>>W
>>W
>=�G�>W
>>W
>=�G�>��R>W
>>W
>>W
>>W
>>��>W
>=�G�>��R>��R>W
>>W
>=�G�>W
>>W
>>W
>>��R>��R=�G�>W
>>W
>>W
>>W
>>��R>��R>W
>>W
>>W
>>��R>W
>>W
>>W
>>W
>>W
>>W
>=�G�>W
>=�G�<#�	>��R>W
>>��R>��R>W
>=�G�>W
>>W
>>W
>>W
>>W
>>W
>>W
>>W
>>W
>>W
>>W
>>W
>>��R>��R>W
>>W
>=�G�=�G�>��R>��R>��R>��R>W
>=�G�=�G�>W
>>W
>>W
>>��R>��R=�G�>W
>>W
>>W
>>W
>>��R>��R>W
>=�G�>W
>=�G�=�G�=�G�>W
>>W
>>W
>>W
>=�G�>W
>>W
>>��R>W
>>W
>=�G�>W
>=�G�=�G�>W
>>W
>>W
>>��R=�G�>��R>��R>��R>��R>��R>��R=�G�>W
>>��R>��R>W
>>W
>>W
>>��R>W
>>W
><#�	=�G�>W
>>W
>>W
>>��>W
>=�G�>W
>>W
>=�G�<#�	>W
>>��R>W
>>W
>>��R>��R>W
>>W
>>W
>>W
>>��R>W
>=�G�>W
>>W
>>W
>>W
>=�G�>W
>>��>��R>W
>>��R>��R=�G�>W
>>W
>>��R>W
>>W
>>W
>>W
>>W
>>W
>=�G�=�G�>��R>W
>=�G�>W
>>��R>W
>>W
>>W
>>��R>W
>>��R>W
>=�G�>W
>>W
>>W
>>��R>��R>W
>>W
>=�G�=�G�>W
>>��R>W
>>W
>>W
>=�G�>W
>>W
>>��R>W
>=�G�>W
>>W
>>W
>>W
>>W
>>W
>>W
>=�G�<#�	>��R>��R>W
>>W
>>��>��?�\?(�?(�?(�?O\)?O\)?h��?�G�?�G�?�{?�{?��?�z�?�z�?�G�?�{?��H?��H?�@ ��@ ��@
=@p�@p�@�
@=p@ ��@'
=@-p�@3�
@@��@G
=@S�
@Z=p@`��@g
=@s�
@z=p@�Q�@��@��@��@�Q�@��@��R@��@�Q�@��@��R@��@�Q�@��@��R@��@��@�Q�@Å@ƸR@��@�Q�@Ӆ@ָR@��@��@�Q�@�R@��@��@�Q�@�@��R@��@��A (�AA��A�\A(�A	A\)A��A(�A(�AA��A�\A(�AA\)A��A�\A!A#\)A$��A&�\A)A+\)A,��A.�\A1A3\)A4��A8(�A9A;\)A<��A>�\AAAC\)AD��AF�\AH(�AK\)AL��AN�\AP(�AQAT��AV�\AX(�AYA\��A^�\A`(�Ac\)Ad��Af�\Ah(�Ak\)Al��An�\Ap(�As\)At��Av�\Ax(�A{\)A|��A~�\A��HA��A�z�A�{A��HA��A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��HA��A�G�A�{A��HA��A�G�A�{A��A�z�A�G�A�{A��HA�z�A�G�A�{A��HA��A�z�A�{A��HA��A�z�A�G�A�{A��HA��A�z�A�{A��HA��A�z�A�G�A�{A��HA�z�A�G�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A�{A��HA��A�z�A�G�A�{A��HA�z�A�G�A�{A��HAŮA�z�A�{A��HAɮA�z�A�G�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��HAծA�z�A�{A��HAٮA�z�A�G�A�{AݮDq �Dq-pDq3�Dq:=Dq@�DqMpDqS�DqZ=Dqg
DqmpDqs�Dq��Dq�
Dq�pDq�=Dq��Dq�
Dq��Dq�=Dq��Dq�
Dq��Dq�=Dq�Dq�pDq��Dq�=Dr
DrpDr�Dr=Dr'
Dr-pDr3�Dr@�DrG
DrMpDrZ=Dr`�Drg
Drs�Drz=Dr��Dr�pDr��Dr�=Dr��Dr�pDr��Dr�=Dr�
Dr�pDr��Dr�=Dr�
Dr�pDr�=Ds �Ds
DspDs=Ds �Ds'
Ds3�Ds:=DsG
DsMpDsS�DsZ=Dsg
DsmpDss�Ds��Ds�
Ds�pDs�=Ds��Ds�
Ds��Ds�=Ds��Ds�
Ds��Ds�=Ds�Ds�pDs��Ds�=Dt
DtpDt�Dt �Dt'
Dt-pDt:=Dt@�DtG
DtS�DtZ=Dt`�DtmpDts�Dtz=Dt�
Dt�pDt��Dt��Dt�
Dt�pDt�=Dt��Dt�
Dt��Dt�=Dt�Dt�pDt��Dt�=Du
Dup@-p�@3�
@@��@G
=@S�
@Z=p@`��@g
=@s�
@z=p@�Q�@��@��@��@�Q�@��@��R@��@�Q�@��@��R@��@�Q�@��@��R@��@��@�Q�@Å@ƸR@��@�Q�@Ӆ@ָR@��@��@�Q�@�R@��@��@�Q�@�@��R@��@��A (�AA��A�\A(�A	A\)A��A(�A(�AA��A�\A(�AA\)A��A�\A!A#\)A$��A&�\A)A+\)A,��A.�\A1A3\)A4��A8(�A9A;\)A<��A>�\AAAC\)AD��AF�\AH(�AK\)AL��AN�\AP(�AQAT��AV�\AX(�AYA\��A^�\A`(�Ac\)Ad��Af�\Ah(�Ak\)Al��An�\Ap(�As\)At��Av�\Ax(�A{\)A|��A~�\A��HA��A�z�A�{A��HA��A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��HA��A�G�A�{A��HA��A�G�A�{A��A�z�A�G�A�{A��HA�z�A�G�A�{A��HA��A�z�A�{A��HA��A�z�A�G�A�{A��HA��A�z�A�{A��HA��A�z�A�G�A�{A��HA�z�A�G�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A�{A��HA��A�z�A�G�A�{A��HA�z�A�G�A�{A��HAŮA�z�A�{A��HAɮA�z�A�G�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��HAծA�z�A�{A��HAٮA�z�A�G�A�{AݮDq �Dq-pDq3�Dq:=Dq@�DqMpDqS�DqZ=Dqg
DqmpDqs�Dq��Dq�
Dq�pDq�=Dq��Dq�
Dq��Dq�=Dq��Dq�
Dq��Dq�=Dq�Dq�pDq��Dq�=Dr
DrpDr�Dr=Dr'
Dr-pDr3�Dr@�DrG
DrMpDrZ=Dr`�Drg
Drs�Drz=Dr��Dr�pDr��Dr�=Dr��Dr�pDr��Dr�=Dr�
Dr�pDr��Dr�=Dr�
Dr�pDr�=Ds �Ds
DspDs=Ds �Ds'
Ds3�Ds:=DsG
DsMpDsS�DsZ=Dsg
DsmpDss�Ds��Ds�
Ds�pDs�=Ds��Ds�
Ds��Ds�=Ds��Ds�
Ds��Ds�=Ds�Ds�pDs��Ds�=Dt
DtpDt�Dt �Dt'
Dt-pDt:=Dt@�DtG
DtS�DtZ=Dt`�DtmpDts�Dtz=Dt�
Dt�pDt��Dt��Dt�
Dt�pDt�=Dt��Dt�
Dt��Dt�=Dt�Dt�pDt��Dt�=Du
DupG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��`A��yA���AȺ^AȰ!AȬAȰ!Aȝ�A�r�A�^5A�Q�A�M�A�E�A�7LA�5?A�33A�1'A�/A�+A�$�A�$�A���A��A��A� �A���AǓuA�G�A� �A�&�A��A���A�?}A���A�7LAƩ�AƇ+AƍPA�x�A�M�Aá�A��A�p�A��jA�\)A��-A��PA�ȴA�l�A���A�
=A�hsA��uA�1'A�  A��RA��A���A�C�A�M�A�JA��A�hsA���A�r�A�t�A�t�A��A�bA��hA��A�A�ĜA��
A�^5A���A���A��DA��yA�bNA���A��RA��9A�Q�A�/A���A�ffA���A��jA���A�-A�ffA�A�`BA�Q�A��/A�ƨA��\A�VA�ƨA�O�A��/A���A���A��A�x�A�Q�A�ĜA�JA���A���A��A���A�ĜA��\A���A�O�A��A���A��A��-A�p�A��A��TA��+A��A��A�-A��PA���A�~�A|r�AxE�AuhsAo7LAl-AhZAf�RAfVAeC�AcG�AbE�A`ȴA^�DA]x�A\ffAY�AW�AVA�AU�
AT�`AR=qAP�!AM��AJ(�AH(�AG�AG�wAG`BAF��AF�/AF�+AD��ACXA@n�A=�FA<�A;x�A:�jA8n�A5p�A3��A2��A1�A0{A.��A-t�A,�A*��A)�TA(VA&n�A%;dA#`BA!�A ��A�;AA  A�wA%A��AbAt�A
=A1A�AA��AXA33A��A�jA�A/A�A�AAS�AA�9A�mAdZA/AA�DA �A�;A+A�7A
�jA	K�A	+A	A��A�A/A5?A�jA�`Ap�A �@�t�@��@��/@�I�@���@��\@���@��h@���@��9@��;@�!@���@��T@�?}@�%@��D@���@��@�o@���@�Q�@� �@� �@�;d@�(�@�r�@�r�@��H@�(�@���@Ӆ@��#@ѩ�@�r�@���@�p�@�b@�^5@�O�@�M�@���@��;@Ǯ@�S�@�E�@���@��@�@���@��@��H@��@�+@�ff@�$�@�M�@�=q@�=q@�x�@�O�@�hs@�G�@�&�@���@�ƨ@��@��^@�hs@�G�@���@�  @�  @�&�@���@���@�$�@��-@�Z@���@�@�ȴ@�;d@���@�A�@���@��@��9@���@�{@�`B@�9X@�33@��y@�
=@�\)@�;d@��@�V@��@�;d@�b@�I�@�;d@��T@�l�@�|�@���@��/@�  @��@��@���@���@�9X@�X@��@���@���@���@��j@�O�@��@�G�@��+@�r�@���@��@�"�@�@�dZ@��R@��#@��@���@�I�@���@�ƨ@��
@�ƨ@��P@��@�ȴ@���@���@�`B@�7L@�V@���@��u@� �@��@���@���@��@���@���@��-@�x�@�7L@���@���@���@���@�V@���@���@��@�I�@� �@���@�K�@�+@�dZ@�l�@��\@��@�X@��@�V@���@���@���@��@��D@��@�z�@�r�@�Q�@�1@�t�@�C�@�o@�o@�
=@���@���@�M�@��T@�&�@��u@��P@�
=@���@�v�@�=q@��T@��-@��@�p�@�p�@�p�@�p�@�p�@�/@�V@���@���@��`@�Ĝ@���@��@�z�@�9X@�A�@�ƨ@��@��w@�33@�~�@�E�@�J@��#@��T@�n�@��!@���@��y@�+@���@���@��!@�n�@��@�/@�%@�V@��@�b@�C�@�33@�+@�+@���@�@��@��@���@�3�@{1�@r� @jl�@bq�@Z�]@P>B@I�@A�#@;�V@7@2�@+e�@''�@ ��@33@K�@�@�q@خG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�C�A�ȴA��A�1A��A���A�7LA�I�A�S�A��^A���Aŝ�A�1A��A��A��A�1A���A��-A��TA�  A�t�A��Aũ�A�A�C�AA�1A�1A���A�33A�S�A���A��FA��A�ȴA�{A�(�A�1A�$�A�9XA��jA�
=A�A�C�A�~�A�A�bNA�
=A���A�ƨAƋDA�33A�n�A���A�A®AƗ�A���ADA�n�A�G�A�Q�A���A��A�  AÅA�M�A�A��
A�+A�{A�n�A���A�  A�$�A���A���A���A�ƨA��TAŃA�ZA���A��A�x�A��TA�(�A��`A�bNA��Aȝ�A�|�A���A�n�A��A��;A�G�A��FA�|�A�A���A���A�ĜA�Q�A�  AȑhA���A��A��A��A��yA��A�r�A���A�-A�/AöFAȲ-A�O�A��
A�1A��A���A�A���Aú^Aţ�A��mA���A�t�A�x�A��A�&�A�Q�AȸRAİ!A�A�"�A�A�"�Að!A���A�Aȉ7A�5?AÏ\Aȉ7A�|�A��A�G�A�Q�A�n�A�bNA�(�A��-A��-A�;dAğ�A�$�AǸRA�33A�ZAƇ+A���A�p�A�r�A���A�bNA���A�p�A�hsA�A���A�VA�E�A�ƨA�A�  AƑhA��#A��;A�VA���A�1A�  A�%A���A���A�A�p�A���A�A�bA��;A��A�%AȍPAȓuA�JA�VA��;A�S�A�dZAȕ�A�bA� �A�  A�bA�1A�A���A��HA�
=A�A�1A�JA�%A�VA�
=A�VA�JA�1A�1A�A�%A�A�{A�{A�%A�A�  A���A�1A�%A�A�  A�%A�A�  A�A�%A��A�%A�VA�{A�%A�
=A�A�{A�JA�1A�oA�bA�bA�VA�oA�A�  A�{A�A�oA�A���A�
=A�A���A���A�%A�A��A�VA��A�A�
=A�  A�1A�VA���A���A�A�%A�VA�  A���A�  A�A�%A���A�A���A���A�t�A�{A�%A�1A�1A�A���A�%A�  A���A�  A���A���A�A���A�1A�%A�A�VA�VA�  A�  A�  A�A�A��A�7LA���A���A���A�  A���A�  A�%A�
=A�%A�1A���A��A��A���A�%A�A���A�1A�A���A�%A���A�%A���A�  A�VA�1A�A�%A�x�A�A�A�A�JA�1A�%A�A�A�bA�1A�1A�1A�%A�bA�JA�A�AȋDA���A���A���A�  A�%A�=qA�  A�1A���A���A���A���A�A�%A�1A�  A��A���A���A���A���A���A���A�1A���A���A�%A���A���A�%A���A�A��A���A���A��A���A���A�%A�  A���A���A��A��A��A���A�"�AȅA�A���A���A���A���A���A�  A���A���A��A���A�1A���A���A���A��A�A��A���A���A��A��A���A���A�A�  A���A��A�1A��A��A�A��A���A���A��A��A��
A��A��A��A���A�A��A�  A��A��A��A��yA��A���A���A���A��yA��A���A�A���A��A�
=A�A���A�%A���A�
=A��A���A�%A���A���A��A��A��A���A��A���A��
A�;dA��A���A���A���A��A��A���A���A��A��mA��mA��A��A��A���A��`A��mA��A��A��;A��;A��mA��TA��yA��A��A���A��mA��A��A��A���A��A��A��A���A���A��A���A��yA��A���A��A��A��A��A���A��A��A���A���A��A���A�  A�
=A���A���A���A���A���A���A���A���A���A���A�  A���A���A���A�  A���A���A�  A���A���A��A��A���A���A���A���A���A���A�ȴA���A���A�ƨA���A���A���A�A���A���A�ȴA���A�ƨA�ĜA�ĜA���AȾwA���A�A�ĜA�AȾwA�ĜAȾwAȺ^Aȴ9AȸRAȼjAȴ9AȸRAȼjAȸRAȺ^AȸRAȺ^AȺ^AȾwAȺ^AȸRAȺ^AȸRAȶFAȶFAȶFAȴ9AȸRAȸRAȲ-Aȴ9AȶFAȶFAȸRAȸRAȺ^Aȴ9AȶFAȴ9AȸRAȸRAȾwAȸRAȺ^AȸRAȼjAȺ^AȼjAȼjAȸRAȼjAȺ^AȾwAȺ^AȺ^AȾwAȺ^AȸRAȰ!Aț�AȶFAȟ�AȃAȁAȁAȃAȁAȅAȃAȃAȃA�z�A�~�A�|�AȃA�|�A�~�A�z�A�x�A�t�A�n�A�n�A�jA�l�A�jA�l�A�l�A�jA�jA�jA�hsA�jA�jA�hsA�dZA�bNA�dZA�`BA�`BA�bNA�`BA�`BA�^5A�\)A�`BA�^5A�XA�\)A�XA�S�A�S�A�VA�VA�VA�VA�VA�S�A�S�A�VA�S�A�XA�VA�S�A�VA�S�A�S�A�XA�XA�S�A�VA�VA�XA�S�A�S�A�XA�S�A�VA�S�A�S�A�S�A�Q�A�Q�@���@���@���@��P@��P@�|�@�l�@�\)@�\)@�S�@�K�@�;d@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�+@�33@�33@�33@�33@�+@�+@�+@�"�@�+@�+@�+@�"�@�+@�+@�"�@�+@�"�@�"�@�33@�;d@�K�@�K�@�S�@�S�@�S�@�S�@�K�@�C�@�;d@�"�@��@�
=@�@��y@��y@��y@��y@��@��@��@��@��@��@���@���@��@���@�@�@�
=@�
=@�o@�o@�o@�o@�o@��@��@��@��@�"�@�o@�@��H@��R@���@���@���@���@��R@���@�ȴ@���@��H@��y@��y@��@���@�@�
=@��@�"�@�"�@�o@��@��@��@�o@�"�@�33A��TA��mA��TA��`A��mA��mA��`A��TA��`A��`A��HA��HA��;A��TA��`A��`A��`A��`A��A��yA��`A��yA��yA��mA��A��A��yA��mA��`A��TA��`A��`A��mA��A��mA��A��A��A��A��A��A��A��`AȾwAȾwAȺ^AȼjAȼjAȺ^AȼjA�A�AȾwA�ĜAȾwAȺ^A�AȾwA�A���A���AȾwAȺ^AȸRAȶFAȸRAȶFAȸRAȶFAȶFAȶFAȼjAȶFAȬAȬAȬAȩ�AȰ!AȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȴ9AȲ-AȲ-AȰ!AȬAȩ�AȬAȬAȬAȮAȬAȬAȮAȩ�AȬAȮAȬAȮAȮAȬAȬAȮAȰ!AȺ^AȲ-AȬAȰ!AȲ-Aȴ9AȰ!Aȴ9AȲ-AȲ-AȲ-AȶFAȶFAȲ-AȮAȲ-AȸRAȲ-Aș�AȓuAȴ9AȁA�x�A�x�A�x�A�x�A�z�A�z�A�z�A�z�A�x�A�t�A�t�A�t�A�x�A�t�A�r�A�n�A�n�A�jA�hsA�l�A�ffA�dZA�dZA�bNA�dZA�bNA�bNA�dZA�bNA�bNA�bNA�\)A�\)A�XA�XA�XA�XA�ZA�ZA�XA�VA�VA�XA�S�A�XA�VA�Q�A�M�A�M�A�O�A�O�A�M�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�M�A�K�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�K�A�K�@���@���@���@���@��P@�|�@�l�@�dZ@�\)@�S�@�S�@�C�@�;d@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�+@�33@�33@�+@�33@�33@�33@�33@�33@�33@�+@�+@�"�@�+@�"�@�+@�+@�"�@�+@�+@�"�@�"�@�+@�+@�33@�C�@�K�@�S�@�S�@�S�@�S�@�K�@�C�@�;d@�+@�"�@�o@�@��@��y@��y@��y@��@��@��@��@��@��@��@���@���@���@�
=@�@�
=@�
=@�
=@�
=@�o@�
=@�o@��@��@��@��@��@��@�@��y@���@��!@���@���@���@��!@��R@�ȴ@�ȴ@��H@��y@��y@��y@��@���@�@�
=@�+@�+@�+@�o@�o@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A��`A��`A��yA���AȺ^AȰ!AȬAȰ!Aȝ�A�r�A�^5A�Q�A�M�A�E�A�7LA�5?A�33A�1'A�/A�+A�$�A�$�A���A��A��A� �A���AǓuA�G�A� �A�&�A��A���A�?}A���A�7LAƩ�AƇ+AƍPA�x�A�M�Aá�A��A�p�A��jA�\)A��-A��PA�ȴA�l�A���A�
=A�hsA��uA�1'A�  A��RA��A���A�C�A�M�A�JA��A�hsA���A�r�A�t�A�t�A��A�bA��hA��A�A�ĜA��
A�^5A���A���A��DA��yA�bNA���A��RA��9A�Q�A�/A���A�ffA���A��jA���A�-A�ffA�A�`BA�Q�A��/A�ƨA��\A�VA�ƨA�O�A��/A���A���A��A�x�A�Q�A�ĜA�JA���A���A��A���A�ĜA��\A���A�O�A��A���A��A��-A�p�A��A��TA��+A��A��A�-A��PA���A�~�A|r�AxE�AuhsAo7LAl-AhZAf�RAfVAeC�AcG�AbE�A`ȴA^�DA]x�A\ffAY�AW�AVA�AU�
AT�`AR=qAP�!AM��AJ(�AH(�AG�AG�wAG`BAF��AF�/AF�+AD��ACXA@n�A=�FA<�A;x�A:�jA8n�A5p�A3��A2��A1�A0{A.��A-t�A,�A*��A)�TA(VA&n�A%;dA#`BA!�A ��A�;AA  A�wA%A��AbAt�A
=A1A�AA��AXA33A��A�jA�A/A�A�AAS�AA�9A�mAdZA/AA�DA �A�;A+A�7A
�jA	K�A	+A	A��A�A/A5?A�jA�`Ap�A �@�t�@��@��/@�I�@���@��\@���@��h@���@��9@��;@�!@���@��T@�?}@�%@��D@���@��@�o@���@�Q�@� �@� �@�;d@�(�@�r�@�r�@��H@�(�@���@Ӆ@��#@ѩ�@�r�@���@�p�@�b@�^5@�O�@�M�@���@��;@Ǯ@�S�@�E�@���@��@�@���@��@��H@��@�+@�ff@�$�@�M�@�=q@�=q@�x�@�O�@�hs@�G�@�&�@���@�ƨ@��@��^@�hs@�G�@���@�  @�  @�&�@���@���@�$�@��-@�Z@���@�@�ȴ@�;d@���@�A�@���@��@��9@���@�{@�`B@�9X@�33@��y@�
=@�\)@�;d@��@�V@��@�;d@�b@�I�@�;d@��T@�l�@�|�@���@��/@�  @��@��@���@���@�9X@�X@��@���@���@���@��j@�O�@��@�G�@��+@�r�@���@��@�"�@�@�dZ@��R@��#@��@���@�I�@���@�ƨ@��
@�ƨ@��P@��@�ȴ@���@���@�`B@�7L@�V@���@��u@� �@��@���@���@��@���@���@��-@�x�@�7L@���@���@���@���@�V@���@���@��@�I�@� �@���@�K�@�+@�dZ@�l�@��\@��@�X@��@�V@���@���@���@��@��D@��@�z�@�r�@�Q�@�1@�t�@�C�@�o@�o@�
=@���@���@�M�@��T@�&�@��u@��P@�
=@���@�v�@�=q@��T@��-@��@�p�@�p�@�p�@�p�@�p�@�/@�V@���@���@��`@�Ĝ@���@��@�z�@�9X@�A�@�ƨ@��@��w@�33@�~�@�E�@�J@��#@��T@�n�@��!@���@��y@�+@���@���@��!@�n�@��@�/@�%@�V@��@�b@�C�@�33@�+@�+@���@�@��G�O�@���@�3�@{1�@r� @jl�@bq�@Z�]@P>B@I�@A�#@;�V@7@2�@+e�@''�@ ��@33@K�@�@�q@خG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�C�A�ȴA��A�1A��A���A�7LA�I�A�S�A��^A���Aŝ�A�1A��A��A��A�1A���A��-A��TA�  A�t�A��Aũ�A�A�C�AA�1A�1A���A�33A�S�A���A��FA��A�ȴA�{A�(�A�1A�$�A�9XA��jA�
=A�A�C�A�~�A�A�bNA�
=A���A�ƨAƋDA�33A�n�A���A�A®AƗ�A���ADA�n�A�G�A�Q�A���A��A�  AÅA�M�A�A��
A�+A�{A�n�A���A�  A�$�A���A���A���A�ƨA��TAŃA�ZA���A��A�x�A��TA�(�A��`A�bNA��Aȝ�A�|�A���A�n�A��A��;A�G�A��FA�|�A�A���A���A�ĜA�Q�A�  AȑhA���A��A��A��A��yA��A�r�A���A�-A�/AöFAȲ-A�O�A��
A�1A��A���A�A���Aú^Aţ�A��mA���A�t�A�x�A��A�&�A�Q�AȸRAİ!A�A�"�A�A�"�Að!A���A�Aȉ7A�5?AÏ\Aȉ7A�|�A��A�G�A�Q�A�n�A�bNA�(�A��-A��-A�;dAğ�A�$�AǸRA�33A�ZAƇ+A���A�p�A�r�A���A�bNA���A�p�A�hsA�A���A�VA�E�A�ƨA�A�  AƑhA��#A��;A�VA���A�1A�  A�%A���A���A�A�p�A���A�A�bA��;A��A�%AȍPAȓuA�JA�VA��;A�S�A�dZAȕ�A�bA� �A�  A�bA�1A�A���A��HA�
=A�A�1A�JA�%A�VA�
=A�VA�JA�1A�1A�A�%A�A�{A�{A�%A�A�  A���A�1A�%A�A�  A�%A�A�  A�A�%A��A�%A�VA�{A�%A�
=A�A�{A�JA�1A�oA�bA�bA�VA�oA�A�  A�{A�A�oA�A���A�
=A�A���A���A�%A�A��A�VA��A�A�
=A�  A�1A�VA���A���A�A�%A�VA�  A���A�  A�A�%A���A�A���A���A�t�A�{A�%A�1A�1A�A���A�%A�  A���A�  A���A���A�A���A�1A�%A�A�VA�VA�  A�  A�  A�A�A��A�7LA���A���A���A�  A���A�  A�%A�
=A�%A�1A���A��A��A���A�%A�A���A�1A�A���A�%A���A�%A���A�  A�VA�1A�A�%A�x�A�A�A�A�JA�1A�%A�A�A�bA�1A�1A�1A�%A�bA�JA�A�AȋDA���A���A���A�  A�%A�=qA�  A�1A���A���A���A���A�A�%A�1A�  A��A���A���A���A���A���A���A�1A���A���A�%A���A���A�%A���A�A��A���A���A��A���A���A�%A�  A���A���A��A��A��A���A�"�AȅA�A���A���A���A���A���A�  A���A���A��A���A�1A���A���A���A��A�A��A���A���A��A��A���A���A�A�  A���A��A�1A��A��A�A��A���A���A��A��A��
A��A��A��A���A�A��A�  A��A��A��A��yA��A���A���A���A��yA��A���A�A���A��A�
=A�A���A�%A���A�
=A��A���A�%A���A���A��A��A��A���A��A���A��
A�;dA��A���A���A���A��A��A���A���A��A��mA��mA��A��A��A���A��`A��mA��A��A��;A��;A��mA��TA��yA��A��A���A��mA��A��A��A���A��A��A��TA��mA��TA��`A��mA��mA��`A��TA��`A��`A��HA��HA��;A��TA��`A��`A��`A��`A��A��yA��`A��yA��yA��mA��A��A��yA��mA��`A��TA��`A��`A��mA��A��mA��A��A��A��A��A��A��A��`AȾwAȾwAȺ^AȼjAȼjAȺ^AȼjA�A�AȾwA�ĜAȾwAȺ^A�AȾwA�A���A���AȾwAȺ^AȸRAȶFAȸRAȶFAȸRAȶFAȶFAȶFAȼjAȶFAȬAȬAȬAȩ�AȰ!AȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȴ9AȲ-AȲ-AȰ!AȬAȩ�AȬAȬAȬAȮAȬAȬAȮAȩ�AȬAȮAȬAȮAȮAȬAȬAȮAȰ!AȺ^AȲ-AȬAȰ!AȲ-Aȴ9AȰ!Aȴ9AȲ-AȲ-AȲ-AȶFAȶFAȲ-AȮAȲ-AȸRAȲ-Aș�AȓuAȴ9AȁA�x�A�x�A�x�A�x�A�z�A�z�A�z�A�z�A�x�A�t�A�t�A�t�A�x�A�t�A�r�A�n�A�n�A�jA�hsA�l�A�ffA�dZA�dZA�bNA�dZA�bNA�bNA�dZA�bNA�bNA�bNA�\)A�\)A�XA�XA�XA�XA�ZA�ZA�XA�VA�VA�XA�S�A�XA�VA�Q�A�M�A�M�A�O�A�O�A�M�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�M�A�K�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�K�A�K�@���@���@���@���@��P@�|�@�l�@�dZ@�\)@�S�@�S�@�C�@�;d@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�+@�33@�33@�+@�33@�33@�33@�33@�33@�33@�+@�+@�"�@�+@�"�@�+@�+@�"�@�+@�+@�"�@�"�@�+@�+@�33@�C�@�K�@�S�@�S�@�S�@�S�@�K�@�C�@�;d@�+@�"�@�o@�@��@��y@��y@��y@��@��@��@��@��@��@��@���@���@���@�
=@�@�
=@�
=@�
=@�
=@�o@�
=@�o@��@��@��@��@��@��@�@��y@���@��!@���@���@���@��!@��R@�ȴ@�ȴ@��H@��y@��y@��y@��@���@�@�
=@�+@�+@�+@�o@�o@��@��@��@��A��TA��mA��TA��`A��mA��mA��`A��TA��`A��`A��HA��HA��;A��TA��`A��`A��`A��`A��A��yA��`A��yA��yA��mA��A��A��yA��mA��`A��TA��`A��`A��mA��A��mA��A��A��A��A��A��A��A��`AȾwAȾwAȺ^AȼjAȼjAȺ^AȼjA�A�AȾwA�ĜAȾwAȺ^A�AȾwA�A���A���AȾwAȺ^AȸRAȶFAȸRAȶFAȸRAȶFAȶFAȶFAȼjAȶFAȬAȬAȬAȩ�AȰ!AȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȴ9AȲ-AȲ-AȰ!AȬAȩ�AȬAȬAȬAȮAȬAȬAȮAȩ�AȬAȮAȬAȮAȮAȬAȬAȮAȰ!AȺ^AȲ-AȬAȰ!AȲ-Aȴ9AȰ!Aȴ9AȲ-AȲ-AȲ-AȶFAȶFAȲ-AȮAȲ-AȸRAȲ-Aș�AȓuAȴ9AȁA�x�A�x�A�x�A�x�A�z�A�z�A�z�A�z�A�x�A�t�A�t�A�t�A�x�A�t�A�r�A�n�A�n�A�jA�hsA�l�A�ffA�dZA�dZA�bNA�dZA�bNA�bNA�dZA�bNA�bNA�bNA�\)A�\)A�XA�XA�XA�XA�ZA�ZA�XA�VA�VA�XA�S�A�XA�VA�Q�A�M�A�M�A�O�A�O�A�M�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�M�A�K�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�K�A�K�@���@���@���@���@��P@�|�@�l�@�dZ@�\)@�S�@�S�@�C�@�;d@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�33@�+@�33@�33@�+@�33@�33@�33@�33@�33@�33@�+@�+@�"�@�+@�"�@�+@�+@�"�@�+@�+@�"�@�"�@�+@�+@�33@�C�@�K�@�S�@�S�@�S�@�S�@�K�@�C�@�;d@�+@�"�@�o@�@��@��y@��y@��y@��@��@��@��@��@��@��@���@���@���@�
=@�@�
=@�
=@�
=@�
=@�o@�
=@�o@��@��@��@��@��@��@�@��y@���@��!@���@���@���@��!@��R@�ȴ@�ȴ@��H@��y@��y@��y@��@���@�@�
=@�+@�+@�+@�o@�o@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=��=��7=Ň�>6g�@�+@�	@�&=�K�>(	�?�D>E1=�=�>!�@��@��>z@�+�@�+@��=��4@`�i@�=�@d=�B�>0|�@�H>?�h>W<@��@�H?Z/�>f*@�&=�[-=�d�?�kf@@��@���>Tu�@�;@��=�f�=���=���?U]�> )�>9��>�b=�}�>�0+@�C>fQ>�T?7O=��4=��0?��&>�B�>WAJ@�X=���@Y�`>��>�4?�P=݈<@)u�=�CB?^N@��@���?O.�>��@]a�@��@��>R?h@a��> n>Q��@�#>]�@#�?�*o?�N�> @�b>\�>�C=�c�=��>v�@�l@H��=�n>�@�g>Ɠ>kQ>
 �? L@��@��=�($>�Z\>Fa@��@�~>z%>�@�~�?^6�>@:?KJ�>5+@�H�@�$=���>�@@_5+@��?��>\C-@�@��?��S=��;=�h�>�d@�b@�H?c�>k�3>�")?�*>)��@�&@�{t=�?�>��@9��@'D=>e@��b@�H@��>O#>���@!�
@�H?#/�=���=�Y=��>�`�@s5=�9=��.=ͩ >'2�?_p@�\=�EN=ɚ�?���@�X@��=��>Y(�@�@�d@�>wAJ@��@�!=��X=�)�>M_�@��@�&@|�?{5+@��D=�+k?�@�!�@�!B@� �@� 2>�G�?�l�>��@��@� �@� �@� �>4/@�#@�"�@��@� @��@��>00>���?���@�!�@� �@��@� �@��@�!@�!>6��@e�m@��@� �@� �@��@�"�@��@�!�@� �@��@�@�!@�!@�!@��@� �@�!�@�!B@� 2@�&@�!�@�!B@� 2@� �@� �@� �@� �@�&@��@� 2@��@�&@�!B@�!@��@�!@��@� 2@�&@� �@�"S@�y@�d@��@� 2@�i@� �@� 2@� �@� �@��@�@��@� �@��@�y@�&@��@�&@�@�y@� �@��@��@� 2@��@� �@��@�y@�i@��@��@�u@� 2@��@�@��@��@�!@x��@�i@�@�y@�&@�i@�@� �@�i@��@�d@��@�@�!B@�y@�u@��@��@�@��@��@�!@��@� @�&@�X@���@�@�y@�u@�X@�X@�&@�&@��@��@�y@��@��@�X@��@�i@�u@�&@�@��@�i@��@�@�&@�@�X@�&@��@�!@��@Q�W@�y@��@�!@��@��@�@�u@��@��@��@�@��@��@��@�@��@�H@��@�T@��@��@�i@��@�@��@��@�i@� @��@�@��@�H@��@��@�;@�;@�X@��@�7@��@��@��@��@��@�H@��@��@��@��@�7@��@�+@��@��@�e�@�@��@��@��@�~@��@��@��@�@)��@��@��@��@�7@��@��@��@�7@�;@��@��@�;@��@�@��@��@��@�&@�+@��@��@��@�7@�~@�n@��@�n@�@�]@��@�@��@�+@�n@�@�n@��@��?F�@�@��@��@��@�n@��@��@�+@��@��@��@�+@��@�;@��@��@�~@�n@��@�7@�i@�;@��@�H@�7@�]@�~@�z@��@�;@��@�;@�@�+@��@�@�M@��@��@p�@�Q@��@��@��@�<@��@�M@��@��@�M@��@�b@�b@��@��@��@��@��@��@��@��@��@��@��@��@�@�r@�b@�r@��@��@�n@�~@�@�	@�;@�~@�~@��@��@��@��@�+@�L@��@�;@��@�	@��@��@��@��@�@��@�	@��@�~@�~@�@�m@�~@��@�&@��@�X@��@�~@�*@�m@�~@��@�@��@��@��@�4@�_@��@��@�@�J@�J@�J@�c@�@��@�|@�@��@�k@��@��@�Z@�@��@�c@�
�@�	-@�	�@�	�@�	�@�	�@�	�@�	�@�	�@�	�@�t@��@��@�@�@�@��@�@��@��@�d@�d@�t@��@�F@��@�t@�d@�!@��@�d@��@�t@��@��@�!@��@��@��@�@�!@�:@��@��@��@�	�@��@�!@��@�	B@��@��@��@��@��@��@��@�	B@�!@��@�y@�O@�G@��;@� *@��a@���@��@��k@��@��@��|@��@��@��@��Z@��Z@��Z@��Z@��@��@���@��R@��>@��@��@��!@���@��!@��!@���@��!@��@��@��@���@��@���@��)@���@��2@���@��6@��@��2@���@���@���@���@��@���@���@���@��m@��m@��@��m@���@��@��@��@��m@��@��@��m@��@��m@��m@��@��m@��m@��m@���@��C@��m@��m@��C@��@���@���@��@��@��@��]@��H@��]@��@Q_�@Q_@Q]y@Q\)@QZ2@QW�@QU�@QUq@QT"@QSz@QQ�@QP�@QP�@QP3@QP3@QP�@QP�@QP�@QP�@QP�@QQ/@QQ/@QQ/@QP�@QP�@QQ/@QQ/@QQ�@QQ�@QQ�@QQ�@QQ�@QQ�@QQ�@QQ�@QQ/@QQ�@QQ�@QR*@QR�@QR�@QR�@QR�@QS&@QS�@QS�@QT"@QU�@QW�@QY�@Q[�@Q[�@Q\S@Q\S@Q\)@Q[-@QY�@QX:@QU�@QTL@QR @QO�@QNf@QM�@QM�@QNf@QO�@QP	@QP�@QQ@QQ/@QQ�@QRT@QR�@QR�@QS�@QT�@QU�@QVC@QW?@QW�@QW�@QX:@QX:@QY�@QZ�@QZ�@QZ�@Q[-@QZ2@QY6@QV�@QSP@QNf@QJ�@QJ�@QKs@QMj@QOa@QQY@QS�@QT�@QX�@QY�@QZ�@Q\�@Q^ @Q_�@Qa�@Qc�@Qe�@Qe�@QeV@Qd�@Qe�@Qe�@Qf�@Qg�@Qh�@Qm�@�0@@�2@�0�@�0�@�2@�2M@�1�@�0@�1�@�0�@�1@�0+@�/�@�1@�2@�1�@�2M@�1@�4Y@�3@�3	@�4�@�4Y@�2�@�4�@�4�@�4�@�3�@�2�@�2�@�28@�2�@�4�@�4�@�4@�5+@�5�@�5@�6�@�7"@�6&@�5�@�3]@�$@�!W@� �@�"S@�"�@�!�@�"�@�$ @�%[@�#�@�&l@�$_@�!W@�$�@�#�@�%@�$ @�$_@�#O@�!B@�!�@� G@� �@� �@� �@� �@� �@� �@�#@� �@�X@��@�X@�C@��@��@��@�:@�:@�&@��@��@��@�u@�u@��@�*@��@�*@�*@�@��@�~@�T@�T@��@�T@��@��@�:@��@�?@�i@�d@�y@�$ @� 2@�&@�!@� �@� �@� 2@� �@�!-@� @� �@�"}@�"}@� q@��@� q@�"h@�!@�n@��@� q@��@��@��@�	@�	W@�	�@�
@�	�@�	�@�	�@��@�t@�t@��@��@�t@��@��@�l@��@�@��@�P@� �@� �@� ?@� ?@���@� �@� i@� ?@���@��.@���@���@��"@���@��"@���@��v@��&@��e@��e@���@��j@��e@��z@���@��	@��	@���@���@���@���@���@���@���@���@��	@��3@��H@��	@��H@���@���@���@���@���@��@���@��@���@��]@��r@��]@���@���@��	@���@���@Qx-@Qu�@Qv6@Qt?@Qr@QqL@Qm�@Qk�@Qj�@Qi@Qh�@Qe�@Qd0@Qd0@Qd@Qd@Qd@Qd@Qc�@Qd0@Qd0@Qd0@Qd�@Qd�@Qd�@Qd0@QdZ@Qd�@Qd�@Qd�@Qd�@Qe@Qe@Qd�@Qe@Qe,@Qd�@Qd0@QdZ@Qd�@Qe,@Qe,@QeV@Qe�@Qe�@Qe�@Qe�@Qe�@QgM@Qi�@Ql�@Qn�@Qpz@Qp�@Qq"@Qq�@Qq"@QnY@Qm�@Qkf@Qh�@Qf�@QdZ@Qag@Q_�@Q_�@Q_�@Qa�@Qbc@Qb�@Qc@Qc5@Qc�@Qd0@Qd�@Qd�@QeV@Qgw@QhI@Qh�@Qj@Qj�@Qj�@Qk�@Qk�@Ql�@Qn�@Qo*@QoT@Qo~@QoT@Qn�@Qk�@Qgw@Qag@Q\}@QZ�@QY�@Q\�@Q^�@Q`�@Qc5@Qc�@QiD@Qj�@Qjj@Qkf@Qm	@Qo~@Qq"@Qr�@Qx-@Qw�@Qz%@Qt@Qt�@Qu@Qw@Qv�@Qv�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         4444333444444334333433444344334434444343344444444434444444434344444443344333434434444444444434443444433444334434444334433443344443344444334444433344434444434444434443344333433444334434433334443333433333344433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333334333333333333333333333333333333333333334333333333333333333333333333333333333333433333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�.@�@�'G�O�G�O�G�O�G�O�G�O�G�O�@��@��G�O�@�+�@�.@��G�O�@`�n@�G�O�G�O�G�O�@�JG�O�G�O�@��@�JG�O�G�O�@�$G�O�G�O�G�O�G�O�@���G�O�@�:@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�[G�O�@Y�eG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@���G�O�G�O�@]a�@��@��G�O�@a��G�O�G�O�@�"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�kG�O�G�O�G�O�@�gG�O�G�O�G�O�G�O�@��@��G�O�G�O�G�O�@��@��G�O�G�O�@�~�G�O�G�O�G�O�G�O�@�H�@�"G�O�G�O�@_5-@��G�O�G�O�@�@��G�O�G�O�G�O�G�O�@�c@�GG�O�G�O�G�O�G�O�G�O�@�'@�{tG�O�G�O�G�O�G�O�G�O�@��b@�J@��G�O�G�O�G�O�@�JG�O�G�O�G�O�G�O�G�O�@s5 G�O�G�O�G�O�G�O�G�O�@�_G�O�G�O�G�O�@�X@��G�O�G�O�@�@�f@�G�O�@��@�%G�O�G�O�G�O�@��@�&G�O�G�O�@��EG�O�G�O�@�!�@�!F@� �@� 4G�O�G�O�G�O�@��@� �@� �@� �G�O�@�#@�"�@��@� @��@��G�O�G�O�G�O�@�!�@� �@��@� �@��@�"@�G�O�@e�m@��@� �@� �@��@�"�@��@�!~@� �@��@�@�"@�@�#@��@� �@�"@�!F@� 3@�(@�!�@�!F@� 6@� �@� �@� �@� �@�&@��@� 4@��@�"@�!H@�%@��@�#@��@� 4@�'@� �@�"R@�z@�c@��@� 3@�j@� �@� 6@� �@� �@��@�@��@� �@��@�|@�&@��@�'@�@�|@� �@��@��@� 2@��@� �@��@�|@�e@��@��@�v@� 6@��@�@��@��@�#@x��@�m@�@�y@�'@�m@�@� �@�j@��@�i@��@�@�!E@�~@�w@��@��@�@��@��@� @��@� !@�&@�[@���@�@�~@�u@�X@�V@�'@�%@��@��@�y@��@��@�Y@��@�k@�v@�(@�@��@�f@��@�@�$@�@�X@�$@��@�"@��@Q�U@�}@��@�#@��@��@�@�z@��@��@��@�@��@��@��@�@��@�J@��@�U@��@��@�j@��G�O�@��@��@�h@�@��@�@��@�J@��@��@�>@�?@�X@��@�9@��@��@��@��@��@�J@��@��@��@��@�7@��@�,@��@��@�e�@�@��@��@��@��@��@��@��@�	G�O�@��@��@��@�9@��@��@��@�8@�>@��@��@�=@��@�@��@��@��@�&@�+@��@��@��@�7@�}@�n@��@�n@�@�`@��@�@��@�+@�o@�@�n@��@��G�O�@�@��@��@��@�o@��@��@�,@��@��@��@�,@��@�>@��@��@�@�n@��@�9@�h@�>@��@�F@�6@�`@��@�z@��@�>@��@�@@�@�)@��@�@�Q@��@��G�O�@�N@��@��@��@�<@��@�L@��@��@�Q@��@�b@�`@��@��@��@��@��@��@��@��@��@��@��@��@�@�r@�c@�u@��@��@�q@�@�@�0C@�2@�0�@�0�@�2@�2K@�1�@�/�@�1�@�0�@�1@�0+@�/�@�1@�2@�1�@�2R@�1@�4X@�3@�3
@�4�@�4Y@�2�@�4�@�4�@�4�@�3�@�2�@�2�@�28@�2�@�4�@�4�@�4@�5*@�5�@�4�@�6�@�7@�6*@�5�@�3`@�$@�!\@� �@�"V@�"�@�!�@�"�@�$!@�%Z@�#�@�&l@�$d@�!Y@�$�@�#�@�%@�$@�$^@�#P@�!@@�!�@� J@� �@� �@� �@� �@� �@� �@�#@� �@�[@��@�X@�F@��@��@��@�9@�6@�$@��@��@��@�u@�x@��@�,@��@�)@�)@�@��@�~@�V@�R@��@�R@��@��@�>@��@�B@�m@�e@�z@�$!@� 4@�&@�%@� �@� �@� 4@� �@�!.@� @� �@�"{@�"~@� r@��@� q@�"f@�!@�n@��@� n@��@��@��@�	@�	W@�	�@�
@�
@�	�@�	�@��@�w@�u@��@��@�t@��@��@�m@��@�@��@�O@� �@� �@� >@� ?@���@� �@� j@� ?@���@��1@���@���@��#@���@��%@���@��w@��&@��h@��f@���@��k@��j@��{@���@��@��@���@���@���@���@���@���@���@���@��@��3@��J@��@��J@���@���@���@���@���@��@���@��@���@��^@��s@��a@���@���@��@���@���@Qx-@Qu�@Qv8@Qt>@Qr@QqM@Qm�@Qk�@Qj�@Qi@Qh�@Qf @Qd+@Qd0@Qd@Qd@Qd@Qd@Qc�@Qd.@Qd0@Qd.@Qd�@Qd�@Qd�@Qd5@Qd[@Qd�@Qd�@Qd�@Qd�@Qe@Qe@Qd�@Qe @Qe-@Qd�@Qd0@Qd^@Qd�@Qe-@Qe-@QeR@Qe�@Qe�@Qe�@Qe�@Qe�@QgM@Qi�@Ql�@Qn�@Qpx@Qp�@Qq"@Qq�@Qq @Qn[@Qm�@Qke@Qh�@Qf�@Qd[@Qah@Q_�@Q_�@Q_�@Qa�@Qbf@Qb�@Qc
@Qc:@Qc�@Qd.@Qd�@Qd�@QeU@Qgv@QhF@Qh�@Qj@Qj�@Qj�@Qk�@Qk�@Ql�@Qn�@Qo-@QoV@Qo}@QoU@Qn�@Qk�@Qgu@Qae@Q\}@QZ�@QY�@Q\�@Q^�@Q`�@Qc3@Qc�@QiH@Qj�@Qjj@Qke@Qm@Qoz@Qq"@Qr�@Qx3@Qw�@Qz(@Qt@Qt�@Qu@Qw
@Qv�@Qv�@�0C@�2@�0�@�0�@�2@�2K@�1�@�/�@�1�@�0�@�1@�0+@�/�@�1@�2@�1�@�2R@�1@�4X@�3@�3
@�4�@�4Y@�2�@�4�@�4�@�4�@�3�@�2�@�2�@�28@�2�@�4�@�4�@�4@�5*@�5�@�4�@�6�@�7@�6*@�5�@�3`@�$@�!\@� �@�"V@�"�@�!�@�"�@�$!@�%Z@�#�@�&l@�$d@�!Y@�$�@�#�@�%@�$@�$^@�#P@�!@@�!�@� J@� �@� �@� �@� �@� �@� �@�#@� �@�[@��@�X@�F@��@��@��@�9@�6@�$@��@��@��@�u@�x@��@�,@��@�)@�)@�@��@�~@�V@�R@��@�R@��@��@�>@��@�B@�m@�e@�z@�$!@� 4@�&@�%@� �@� �@� 4@� �@�!.@� @� �@�"{@�"~@� r@��@� q@�"f@�!@�n@��@� n@��@��@��@�	@�	W@�	�@�
@�
@�	�@�	�@��@�w@�u@��@��@�t@��@��@�m@��@�@��@�O@� �@� �@� >@� ?@���@� �@� j@� ?@���@��1@���@���@��#@���@��%@���@��w@��&@��h@��f@���@��k@��j@��{@���@��@��@���@���@���@���@���@���@���@���@��@��3@��J@��@��J@���@���@���@���@���@��@���@��@���@��^@��s@��a@���@���@��@���@���@Qx-@Qu�@Qv8@Qt>@Qr@QqM@Qm�@Qk�@Qj�@Qi@Qh�@Qf @Qd+@Qd0@Qd@Qd@Qd@Qd@Qc�@Qd.@Qd0@Qd.@Qd�@Qd�@Qd�@Qd5@Qd[@Qd�@Qd�@Qd�@Qd�@Qe@Qe@Qd�@Qe @Qe-@Qd�@Qd0@Qd^@Qd�@Qe-@Qe-@QeR@Qe�@Qe�@Qe�@Qe�@Qe�@QgM@Qi�@Ql�@Qn�@Qpx@Qp�@Qq"@Qq�@Qq @Qn[@Qm�@Qke@Qh�@Qf�@Qd[@Qah@Q_�@Q_�@Q_�@Qa�@Qbf@Qb�@Qc
@Qc:@Qc�@Qd.@Qd�@Qd�@QeU@Qgv@QhF@Qh�@Qj@Qj�@Qj�@Qk�@Qk�@Ql�@Qn�@Qo-@QoV@Qo}@QoU@Qn�@Qk�@Qgu@Qae@Q\}@QZ�@QY�@Q\�@Q^�@Q`�@Qc3@Qc�@QiH@Qj�@Qjj@Qke@Qm@Qoz@Qq"@Qr�@Qx3@Qw�@Qz(@Qt@Qt�@Qu@Qw
@Qv�@Qv�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         4444333444444334333433444344334434444343344444444434444444434344444443344333434434444444444434443444433444334434444334433443344443344444334444433344434444434444434443344333433444334434433334443333433333344433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333334333333333333333333333333333333333333334333333333333333333333333333333333333333433333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�96�k96�`96�496�96�`96ϝ96�A96�"96� 96�496�L96�Q96��96�I96�Z96� 96ϥ96�I96��96Ё96�l96�/96��96�96�a96�96�96�96��96�96ω96�&96�96�96�|96ҷ96ӂ96҈96Ԕ96��96��96�?96��96�;96�Q96��96�_96��96�96��96�O96��96��96��96��96�N96�96��96�b96�L96��96�n96�396��96�)96�h96�l96��96��96��96�l96�$96��96��96��96��96��96�S96��96�a96��96��96��96�N96�Q96�X96�C96�F96��96��96�l96��96��96��96�d96�$96��96��96�n96��96�c96�>96��96��96��96�96�96�496�O96�96��96��96��96��96�96��96� 96��96��96��96��96�T96��96�S96�q96��96��96��96�P96�396��96��96�96�^96��96�,96�96�96��96��96�W96�U96��96�j96�T96�O96�x96�96�96��96�96��96��96�)96��96��96�596��96��96��96�96�f96�96��96�.96��96�096��96��96�96�d96�b96��96�S96�f96�y96��96��96��96�J96�I96�c96�i96��96�g96�f96�f96��96��96�96��96�96�J96�`96�`96��96��96��96��96��96�`96�96�296�96�G96�I96��96��96�|8�S8�P�8�P�8�N�8�L�8�K�8�G�8�E�8�D�8�B�8�BH8�?t8�=y8�=~8�=Q8�=P8�=Q8�=S8�=%8�=|8�=~8�=|8�>28�>:8�>	8�=�8�=�8�=�8�=�8�>8�>28�>a8�>d8�>48�>_8�>�8�>	8�=~8�=�8�>78�>�8�>�8�>�8�?8�?8�?8�?8�?p8�@�8�C�8�F�8�H�8�J�8�K8�K{8�L8�Ky8�H{8�G�8�EH8�B�8�@'8�=�8�:}8�8�8�8�8�8�8�:�8�;�8�;�8�<A8�<t8�<�8�=|8�>48�>18�>�8�A8�A�8�Bw8�C�8�D�8�D�8�Es8�Ex8�F�8�H�8�I^8�I�8�I�8�I�8�I 8�E�8�A8�:z8�5-8�3i8�2\8�5�8�7u8�9�8�<m8�<�8�B�8�D�8�D98�EH8�G8�I�8�K{8�M8�S8�R�8�U;8�N�8�O-8�O�8�Q�8�QR8�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbBbBbBbBbBbBbB\B\B\B\B\B\B\B\B\B\B\BbBhB{BC�B��B�FBȴB��B��B��B�3B�B�B�B��B�VB�7B��BBŢB�B�NB�#B�PBXB`BB�B�B�%B�{B�B�B@�BQ�BQ�B=qB?}BVBgmBA�B0!BoB�B$�B"�B,B(�B�B%B�B�;B��B$�BhBJB33B�/BBbBbB
=B%BB%B
=BoB�B�B�B\BB��B�5B�B��BƨB�}B�B��B�VB�JB�7B~�B`BBW
BO�B2-B�B{B�B�fB��BBB��BB	7BB��B�B�NB�
BŢB��B�1BT�BE�B<jB �BJB
��B
��B
��B
dZB
@�B
�B
B	��B	�}B	�B	��B	��B	��B	�DB	�B	s�B	aHB	XB	L�B	8RB	'�B	 �B	�B	�B	�B	PB��B�B�;B�/B�#B�B�B��B��BȴB�qB�B��B��B��B��B�=B{�Bu�Br�Bp�Bo�Bn�Bk�Bn�Bl�BhsBgmBcTBcTBjBl�Bl�BjBiyBiyBhsBhsBgmBgmBe`BcTBbNBbNBbNBbNBbNBbNBbNBaHB`BB`BBdZBffBjBo�Bq�Bp�Bm�Bl�Bl�Bk�BjBiyBffBbNB`BB_;BhsBhsBgmBe`BaHBjBhsBffBcTB^5BYBW
BT�BS�BR�BQ�BQ�BQ�BP�BN�BO�BS�BYBe`Bl�Bo�Br�Bs�Bp�Bn�Bk�BhsBe`Be`BgmBe`B]/BQ�BM�BL�BJ�BQ�BP�BS�BW
BZBZBYBZB[#B_;Bn�Bl�BjBl�Bs�Bs�Bt�Bt�By�B~�B�B�B�1B�PB�\B�{B��B��B��B��B��B�!B�9B�FB�RB�FB�9B�-B�'B�'B�LB�wBB��B��B�
B�B��B��B��B��B��B�#B�HB�yB�B�B�B�B�B�B�sB�ZB�ZB�fB�yB�B�yB�yB�mB�B��B��B��B��B�B��B��B��B	B	B	B	1B	+B		7B	hB	�B	"�B	1'B	33B	.B	#�B	+B	/B	6FB	1'B	(�B	'�B	,B	-B	0!B	1'B	1'B	0!B	2-B	49B	49B	6FB	8RB	9XB	;dB	;dB	>wB	B�B	B�B	C�B	C�B	C�B	B�B	D�B	F�B	I�B	J�B	K�B	L�B	M�B	N�B	N�B	P�B	R�B	T�B	T�B	T�B	W
B	[#B	_;B	dZB	hsB	iyB	jB	k�B	l�B	p�B	s�B	t�B	w�B	w�B	w�B	z�B	z�B	{�B	|�B	~�B	�B	�%B	�%B	�%B	�+B	�=B	�VB	�hB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�?B	�FB	�FB	�FB	�RB	�jB	��B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B
MB
�B
�B
/B
'�B
0;B
:�B
D�B
J�B
M�B
UgB
X_B
[�B
^�B
bB
f�B
n�B
s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��>��>���?gϢB
�jB
�FBC>�#�?Y(J@Y��?��w?��?L��B
�BHr?@��A��"B
�jB
�?n"A�l)B�>�\�>���?`�B
�?z�H?�-B
��B
�B@�9c?�D9B��>�m?M@ᔼA�:�A�u?�k�B
�nBG>���>�c�?�%@��c?&�m?t�?�i�?��?ِ�B
�}?%�y@�6@�*�>�L�? W@�u@ Z?���B
��?ՕA��0?=�J@)�fAOv?��A���? �r@�%�B
�PB
�@�Ż?J��A��B
�-B�?��A�U�?&_�?�p�BM@?&��A{v8@�#D@�P�?<��Am�<?�x?�<�>ς�>�\S?"��A�0UA�Q�?
_�?>�EB
�Y?0�@?G��?0<K@-�2B
�BB
��?$�?�n�?BB
�*BQ?Gw:?7q�B O�@�4�?8��@��g?DXA�B4�>�?0�]A��B6@1�q?��B
�JB
�m@�#�>ϓ�>�_p?78�B
�sB
�U@���?��_@=�@��J?X��BA���>�V?+��A�p�A�{n?:�B�^B
�Bt?���?��~AubB$�@g2?#�>���>��?�,�A�Ǫ>Ҏ�?�?�?TZq@6��Bp�>��>�*@�*B
�B,�?!G�?�^�B0�B
�B+<?�[�B
��B
�#>с|?4U?��YB
��B
�A\@��Bc�?�5@K�nB
�B
��B
�DB
��@w�Al/?4@JB �oB
�B
�;B6?@��B
�MB%�A��B
�/B
�,B
�V?`��?���A9u6B
�RB
��B
��B
�NB
�wB
�$B
�?n��A��B
��B
�pB
��B
�QB
�B
�hB
�B
��B
�oB
��B
�$B
�B
�cB
��B
�cB
�@B
�B
��B
��B
�B
��B
�_B
�HB
�4B
��B
�JB
�<B
�HB
�B
�QB
�B
�B
�B
�B
�,B
�zB
�:B
��B
�,B
�B
�B
��B
�B
�gB
�VB
�B
�_B
�7B
��B
�mB
��B
��B
��B
�(B
��B
�3B
��B
�B
�|B
��B
�dB
��B
�B
�qB
��B
�;B
��B
��B
��B
��B
�uB
�3B
�_B
�B
�mB
��B
�qB
�RAśTB
�B
�B
�6B
��B
��B
��B
��B
�_B
�B
�GB
�NB
��B
�[B
��B
�B
�DB
��B
�yB
��B
��B
��B
��B
�B
�{B
�A�ZB
�eB
��B
�B
�[B
��B
�B
�B
��B
�PB
�6B
��B
�dB
�B
�B
�B
�zB
��B
��B
�B
��B
�PB
�qB
�B
��B
�[B
�B
�)B
�$B
�UA��RB
��B
��B
�cB
�B
�B
�B
��B
��B
�bB
��B
��B
�B
�hB
�~B
�JB
�5B
�B �B
��B
��B
�uB
�_B
�AE�B
�B
��B
� B
�#B
��B
��B
�B
�B
�B
��B
��B
�B
��B
�^B
�,B
�ZB
�uB
�B
��B
�B
�B
�~B
�B
�dB
��B
��B
��B
�GB
�B
�A�ʎB
�B
��B
�B
�>B
��B
��B
�B
��B
�A�>�B
�B
�B
�B
��B
�B
�qB
�B
�kB
�qB
�B
��B
��B
��B
�BB
��B
�(B
��B
��B
��B
��B
�B
�B
��B
�'B
�B
�|B
��B
��B
��B
��B
�gB
�TB
�B
��B
��B
�sB
��B
��@@�.B
�JB
�oB
��B
�B
�B
�B
�4B
��B
�gB
�5B
��B
��B
�B
�qB
�'B
��B
�B
��B
�|B
�aB
�B
�B
��B
��B
�B
�B
��B
�=B
�ZB
�'B
�B
�B
�BB
�>B
�B
��B
�*B
�,B
�AVaB
�B
�yB
��B
�B
�B
�B
�B
�B
�B
�.B
�B
�B
��B
��B
��B
�/B
�jB
��B
�B
�vB
�nB
��B
�B
�B
�AB
�B
�IB
��B
�'B
��B
�GB
�GB
�)B
�B
��B
�>B
�B
�B
�B
��B
��B
�B
�B
��B
��B
��B
�>B
��B
��B
�B
�B
��B
��B
�/B
�B
�B
�B
�B
��B
��B
��B
�B
�B
�,B
��B
��B
��B
�B
�zB
�IB
� B
�aB
�B
�#B
�B
�B
�#B
�	B
��B
�'B
�KB
��B
�iB
�CB
��B
�,B
��B
�&B
��B
��B
�xB
��B
��B
�B
��B
��B
��B
�FB
�B
��B
�4B
�4B
�bB
�6B
��B
�hB
��B
��B
��B
��B
�B
�B
�FB
��B
�B
�\B
��B
��B
��B
�(B
�xB
��B
��B
�bB
��B
�|B
�B
��B
�-B
��B
��B
�QB
�2B
�%B
�OB
�jB
��B
�BB
��B
��B
�B
��B
�_B
�BB
�B
�"B
�9B
��B
��B
�B
�BB
��B
��B
�kB
��B
��B
�B
�B
�B
�B
�B
�AB
�B
�B
��B
��B
�=B
�_B
�ZB
��B
�+B
�tB
��B
��B
��B
�B
�B
�B
�B
�/B
��B
�B
�$B
�tB
��B
��B
��B
��B
�B
�B
�pB
��B
�`B
�[B
�CB
��B
��B
��B
��B
��B
��B
�B
�*B
��B
��B
��B
��B
�B
�sB
��B
�>B
��B
�dB
��B
��B
��B
��B
��B
��B
�kB
��B
�gB
�B
��B
�B
��B
��B
�B
�KB
�bB
��B
��B
��B
�B
��B
� B
��B
�B
��B
�)B
�B
��B
�6B	�LB	��B	��B	��B	�6B	�nB	�B	��B	��B	�]B	��B	�zB	�?B	��B	��B	�B	�;B	�!B	��B	�B	�7B	�B	�B	��B	��B	��B	��B	��B	��B	�#B	�B	��B	��B	��B	��B	�@B	�pB	��B	��B	�B	�B	��B	��B	�B	�~B	��B	��B	��B	�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�GB	��B	��B	�4B	�'B	��B	�pB	��B	�B	�@B	�QB	��B	�B	�!B	�QB	��B	B	��B	�YB	��B	�$B	�B	�hB	�[B	�CB	�B	�B	��B	�B	�RB	�}B	��B	�DB	��B	B	�bB	��B	�TB	ǨB	��B	�oB	�GB	�B	ǉB	��B	�iB	�SB	�JB	ȼB	��B	�FB	�HB	��B	�iB	�#B	�'B	ȕB	�nB	�B	�wB�BB�B�B�BB�ByB6B�BB@B�B3BJB�BtB@B�B�B�B�B�B�BTB�B�BuBMB"B�BSBB�B�BB�B�B�BB�BBBxB9B�B�BSB|B�B�BwB�B�B�B$B�B�BTBB�B B�B�B�BB�BFB�BhBLBB	B>BB�B�B�B�B�BXB�B�B�BBBaBBBSB+B�BBB�B�B>BB5BLB�BJBB�BlB�B�B�BBRB�B=B�B!BOB]B6BkBKB�B�B�B�B�BjB�B�BfB�BgBBWBOByB�BEBwB[BSB�B�B5B-B�B0B�B�B�BBB�B�BoB�B�BRBB�B�B!B�B�B9BB�B�BRB�BaB2B�B�B�B]BwB�B�B�B�BxBB
B	BB B�B�B�B�B<BGB�B6B�BkBcB�BqB�B�B�B)B�B�B�B�B�B1BB�B	�5B	�}B	ʭB	�@B	ɴB	� B	�qB	�"B	�~B	�/B	��B	��B	ɑB	ʔB	�[B	�NB	�AB	�&B	��B	�+B	�B	�B	�qB	�dB	�+B	��B	��B	��B	��B	��B	��B	��B	��B	ɹB	ɾB	��B	�vB	�B	�B	�oB	ʃB	�vB	˗B	ʪB	ʝB	ˠB	˒B	ʦB	ˎB	�\B	�KB	̞B	̴B	��B	�B	�VB	��B	��B	�pB	��B	��B	�dB	��B	ͦB	�uB	�[B	�mB	͂B	�B	�2B	�bB	�tB	ΗB	�B	�bB	�HB	ΗB	��B	ψB	��B	ϯB	�<B	�!B	�B	ЂB	�MB	ЁB	��B	��B	��B	лB	�DB	�?B	�$B	��B	�dB	�CB	�qB	�}B	ѱB	��B	ѵB	�B	��B	��B	�zB	�B	�.B	��B	��B	��B	ӤB	�.B	��B	ӠB	��B	�!B	ԄB	�B	�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444333444444334333433444344334434444343344444444434444444434344444443344333434434444444444434443444433444334434444334433443344443344444334444433344434444434444434443344333433444334434433334443333433333344433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333334333333333333333333333333333333333333334333333333333333333333333333333333333333433333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 BXBWBXBYB[BXBYBQBSBRBTBPBRBRBUBSBSBQB[BbBpBC�B�xB�:BȬB��B˽B�yB�+B�B�B��B��B�OB�.B��BBŘB��B�GB�B�FBXB`:B�B�B�B�rB�B�B@xBQ�BQ�B=iB?qBU�BgdBA�B0BeB�B$�B"�B,B(�B�BB�xB�2B��B$�BbBDB3)B�&BB[B\B
5BBBB
/BdB�B�B�BSB �B��B�(B��B��BƛB�qB�B��B�MB�BB�)B~�B`:BV�BO�B2B�BrB�B�[B��B �BB��BB	.BB��B�B�FB��BŘB��B�'BT�BE�B<`B �BAB
��B
��B
�zB
dMB
@zB
�B
B	��B	�rB	��B	��B	��B	��B	�:B	� B	s�B	a<B	XB	L�B	8GB	'�B	 �B	�B	�B	�B	DB��B�rB�/B�%B�B�B��B��B��BȩB�eB��B��B�zB��B��B�.B{�Bu�Br�Bp�Bo�Bn�BkxBn�BlBheBgbBcIBcHBjtBl�BlBjrBilBimBhdBhfBg^BgbBeUBcFBbCBb@BbCBbABbABbABbABa<B`2B`5BdKBfZBjoBo�Bq�Bp�Bm�Bl}BlBkwBjqBilBfYBbBB`6B_-BheBhfBgaBeTBa=BjpBhfBfYBcFB^)BYBV�BT�BS�BR�BQ�BQ�BQ�BP�BN�BO�BS�BY	BeRBl~Bo�Br�Bs�Bp�Bn�BkyBhhBeQBeRBg`BeRB]!BQ�BM�BL�BJ�BQ�BP�BS�BV�BZBZBYBZB[B_-Bn�BlBjoBl}Bs�Bs�Bt�Bt�By�B~�B��B�B�#B�CB�OB�mB��B��B��B��B��B�B�+B�8B�EB�9B�-B� B�B�B�?B�iBB˸B��B��B��B��B��B��B��B��B�B�8B�kB�~B�B�B�B�B�B�fB�MB�KB�XB�iB�qB�lB�lB�`B�B��B��B��B��B�B��B��B��B	�B	B		B	$B	B		'B	[B	}B	"�B	1B	3%B	.B	#�B	*�B	/B	66B	1B	(�B	'�B	+�B	,�B	0B	1B	1B	0B	2!B	4-B	4*B	68B	8BB	9KB	;WB	;TB	>jB	BB	B~B	C�B	C�B	C�B	B�B	D�B	F�B	I�B	J�B	K�B	L�B	M�B	N�B	N�B	P�B	R�B	T�B	T�B	T�B	V�B	[B	_-B	dKB	hdB	imB	jqB	kvB	l{B	p�B	s�B	t�B	w�B	w�B	w�B	z�B	z�B	{�B	|�B	~�B	�
B	�B	�B	�B	�B	�/B	�KB	�ZB	�XB	�^B	�`B	�`B	�lB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�/B	�8B	�7B	�6B	�DB	�[B	�zB	ŒB	ȥB	ʲB	˹B	��B	��B	��B	��B	��B	̾B	˹B	˸B	��B	��B	˹B	ʴB	ʴB	ʴB	��B	��B	��B	��G�O�B	��B	�B	��B
@B
�B
�B
 B
'�B
0,B
:�B
D�B
J�B
M�B
UZB
XPB
[�B
^�B
bB
f�B
n�B
stG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�cB
�?B9G�O�G�O�G�O�G�O�G�O�G�O�B
�BHiG�O�A��B
�cB
�G�O�A�l"B�G�O�G�O�G�O�B
�G�O�G�O�B
��B
�=G�O�G�O�B��G�O�G�O�G�O�G�O�A�dG�O�B
�cBGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�sG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�A��'G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�HB
�G�O�G�O�A��	B
�"B�G�O�A�U�G�O�G�O�BM6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�0FG�O�G�O�G�O�B
�NG�O�G�O�G�O�G�O�B
�9B
��G�O�G�O�G�O�B
�BJG�O�G�O�B O�G�O�G�O�G�O�G�O�A�B4�G�O�G�O�A��B6vG�O�G�O�B
�AB
�bG�O�G�O�G�O�G�O�B
�jB
�LG�O�G�O�G�O�G�O�G�O�B�A���G�O�G�O�G�O�G�O�G�O�B�UB
�BmG�O�G�O�G�O�B$�G�O�G�O�G�O�G�O�G�O�A�ǚG�O�G�O�G�O�G�O�G�O�Bp�G�O�G�O�G�O�B
��B,�G�O�G�O�B0�B
�B+1G�O�B
��B
�G�O�G�O�G�O�B
��B
�G�O�G�O�Bc�G�O�G�O�B
�uB
��B
�<B
��G�O�G�O�G�O�B �fB
�B
�.B0G�O�B
�GB%�A���B
�&B
�%B
�MG�O�G�O�G�O�B
�JB
��B
��B
�DB
�mB
�B
�G�O�A��B
��B
�hB
��B
�HB
�B
�bB
�B
��B
�eB
�B
�B
�~B
�ZB
��B
�YB
�:B
�B
��B
��B
�B
��B
�XB
�>B
�,B
��B
�AB
�2B
�?B
��B
�HB
�B
�B
�B
�B
�#B
�qB
�4B
��B
� B
�B
�	B
�B
�B
�]B
�LB
�B
�XB
�.B
��B
�cB
��B
��B
��B
�!B
��B
�+B
��B
�B
�tB
��B
�[B
��B
�B
�hB
��B
�1B
��B
��B
��B
��B
�lB
�(B
�XB
��B
�cB
��B
�kB
�JAśHB
�{B
�B
�-B
��B
�B
��B
��B
�VB
�B
�BB
�BB
��B
�RB
�|B
�B
�8B
��B
�pB
��B
�B
��B
��B
�B
�sB
�vA�GB
�]B
�|B
��B
�PB
��B
�B
�B
�B
�JB
�-B
��B
�\B
�B
�B
��B
�qB
��B
��B
�B
��B
�JB
�fB
�B
��B
�PB
�zB
� B
�B
�NA��DB
��B
��B
�ZB
�B
��B
�B
�zB
��B
�XB
��B
��B
�B
�`B
�tB
�BB
�,B
�B �B
��B
��B
�lB
�VB
�G�O�B
�B
��B
�B
�B
��B
��B
��B
�B
��B
�B
��B
�B
��B
�TB
�"B
�NB
�jB
�B
��B
�B
�B
�vB
�B
�ZB
��B
��B
�}B
�=B
�B
��A��{B
�B
�B
�B
�5B
��B
��B
�B
��B
��G�O�B
� B
��B
�B
��B
�B
�eB
�B
�cB
�iB
�B
�}B
��B
��B
�<B
��B
�"B
�}B
��B
��B
��B
�B
�	B
��B
�B
�B
�sB
��B
��B
��B
��B
�]B
�JB
�B
��B
��B
�jB
��B
��G�O�B
�AB
�fB
��B
�B
�B
�B
�*B
��B
�]B
�-B
��B
��B
�B
�iB
�B
��B
�B
��B
�sB
�[B
�B
�B
��B
��B
�B
�B
��B
�5B
�QB
�B
�B
�B
�<B
�4B
��B
��B
�#B
�$B
��G�O�B
��B
�oB
��B
�B
�B
�B
�B
�B
�|B
�&B
�wB
�B
��B
��B
��B
�(B
�aB
�B
�B
�mB
�eB
��B
�B
�|B
�7B
�B
�>B
��B
�B
��B
�?B
�>B
�!B
�B�BB�B�B�BBBqB-BvBB7B�B)BBB�BmB7B�BxB�B�BzB�BNB�B~BlBGBB�BJBB�B�B B�B�B�BB�B6BqB0B�B�BKBsB�B�BpB�B�B�BB�B�BKBB�BB�B�B�BByB=B�BaBDBB�B7BB}B�B�B�B�BNB�B�B�BB�BXBB�BJB#B�BBB�B�B5B�B+BBB�BBB	B�BbB�B�B�BBJB�B5ByBBFBTB-BaBAB�B�B�ByB�B`B�B�B]B�B\BBRBGBqB�B;BoBRBIB�B�B,B&B�B'B�B�B�BBB�B�BfB�B�BHB	B�B�BB�B�B2BB�B�BIB�BYB*B�B�B�BRBoB�B�B�ByBpB
B B�B�BB�B�B�B�B3B>B�B.B�BaB[B�BhB�B�B�B!B�B�B�B�B�B*B
B�B	�&B	�nB	ʠB	�1B	ɥB	�B	�bB	�B	�mB	� B	ɹB	��B	�B	ʇB	�LB	�?B	�2B	�B	��B	�B	�B	��B	�aB	�YB	�B	��B	��B	��B	��B	��B	��B	��B	��B	ɪB	ɮB	��B	�gB	�B	�B	�bB	�sB	�gB	ˇB	ʝB	ʍB	˒B	˂B	ʘB	�B	�MB	�;B	̌B	̤B	��B	��B	�IB	��B	��B	�aB	��B	��B	�VB	ͺB	͗B	�fB	�MB	�^B	�sB	�B	�$B	�TB	�gB	ΈB	��B	�SB	�8B	ΈB	��B	�xB	κB	ϠB	�-B	�B	�oB	�uB	�=B	�qB	��B	��B	��B	ЭB	�6B	�0B	�B	��B	�UB	�5B	�eB	�pB	ѢB	��B	ѦB	��B	��B	��B	�kB	�
B	�B	��B	��B	��B	ӘB	�B	��B	ӑB	��B	�B	�uB	��B	�-B�BB�B�B�BBBqB-BvBB7B�B)BBB�BmB7B�BxB�B�BzB�BNB�B~BlBGBB�BJBB�B�B B�B�B�BB�B6BqB0B�B�BKBsB�B�BpB�B�B�BB�B�BKBB�BB�B�B�BByB=B�BaBDBB�B7BB}B�B�B�B�BNB�B�B�BB�BXBB�BJB#B�BBB�B�B5B�B+BBB�BBB	B�BbB�B�B�BBJB�B5ByBBFBTB-BaBAB�B�B�ByB�B`B�B�B]B�B\BBRBGBqB�B;BoBRBIB�B�B,B&B�B'B�B�B�BBB�B�BfB�B�BHB	B�B�BB�B�B2BB�B�BIB�BYB*B�B�B�BRBoB�B�B�ByBpB
B B�B�BB�B�B�B�B3B>B�B.B�BaB[B�BhB�B�B�B!B�B�B�B�B�B*B
B�B	�&B	�nB	ʠB	�1B	ɥB	�B	�bB	�B	�mB	� B	ɹB	��B	�B	ʇB	�LB	�?B	�2B	�B	��B	�B	�B	��B	�aB	�YB	�B	��B	��B	��B	��B	��B	��B	��B	��B	ɪB	ɮB	��B	�gB	�B	�B	�bB	�sB	�gB	ˇB	ʝB	ʍB	˒B	˂B	ʘB	�B	�MB	�;B	̌B	̤B	��B	��B	�IB	��B	��B	�aB	��B	��B	�VB	ͺB	͗B	�fB	�MB	�^B	�sB	�B	�$B	�TB	�gB	ΈB	��B	�SB	�8B	ΈB	��B	�xB	κB	ϠB	�-B	�B	�oB	�uB	�=B	�qB	��B	��B	��B	ЭB	�6B	�0B	�B	��B	�UB	�5B	�eB	�pB	ѢB	��B	ѦB	��B	��B	��B	�kB	�
B	�B	��B	��B	��B	ӘB	�B	��B	ӑB	��B	�B	�uB	��B	�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444333444444334333433444344334434444343344444444434444444434344444443344333434434444444444434443444433444334434444334433443344443344444334444433344434444434444434443344333433444334434433334443333433333344433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333334333333333333333333333333333333333333334333333333333333333333333333333333333333433333333333333333333333333333311112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647072020083116470720200831164707202008311647072020083116470720200831164707202008311647072020083116470720200831164707202008311647072020083116470720200831164707AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816322019021918163220190219181632    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816322019021918163220190219181632  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816322019021918163220190219181632  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647072020083116470720200831164707  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                