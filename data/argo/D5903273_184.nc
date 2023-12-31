CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  O   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:24Z creation      
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
resolution        =���   axis      Z        '�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  v�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  �X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� )d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 3T   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� [   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �`   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ?\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� IL   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � q    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   }�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181724  20200831164948  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�|O��@�|O��@�|O��111 @�|OhK�@�|OhK�@�|OhK�@6��Q�@6��Q�@6��Q��cC-�cC-�cC-111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�33@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  DtffDy]qD�HD�>D���D��D�  D�B=D���D��RD��D�:=D���DǸ D���D�#�D�vD�3D��D�:�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����;������;��;��;��;��������������;������;��������������;������;��;��;��;��;���    =���    �����������������������;��;L�;������������������;������;����L�;��;������������������;����������;��;������;��;����������;����������;��������������;����������;����L�;������;L�;������;����������;����L�;��;��;��������������;��;������;������;��;��;��������L�;��;��;����L�;������;����L�;L�;������;������;��;L��    �L�;��;���    ���;��;L�;��;��;����L�;������;��;L�ͽ��;��;������;��;������;��;������;��;����L�;��;��ͽ��;L�;��;��;����L�;������;��;��;����L��    �������;��;��;��;������;������;��;����L�;L�;��;��;L�;L�;��;��;��;L�ͽ��;L�;��;��;L�;����L��    ���;��������������;L�ͽ��;L�;����������������L�ͽ��;������;����L�ͽ��;��������L��=��;L�;��;��ͽ��ͽ���        ���;��;��;����L�;L�ͽ��;L�;L�;L�;��;L�;L�;L�;L�;L�;������ͽ��;L��    �L�;L�ͽ��;L�;L�ͽ���    �L�ͽ���    ���ͽ��ͽ��ͽ��;����L�ͽ���    ���ͽ��;L�;L��    ���;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ���        �L�ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ��;L��    ���ͽ��ͽ���                ���ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ��;������ͽ���    ���;L�;L�ͽ���        ����    �L�ͽ��ͽ��ͽ��ͽ���            �L�;������ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��ͽ���        ���;L�ͽ��;������ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ���    �L�;L�ͽ��;L�ͽ��ͽ���=���    ���ͽ��ͽ���    =���=��ͽ��;L�;����L�ͽ��;L��    ���;L�ͽ��ͽ��ͽ��ͽ��;L�;L�;L�ͽ���    �L�ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��;L�ͽ��ͽ���    ���ͽ���    �L�;L�ͽ��;L�ͽ��ͽ���        ���;L�ͽ��ͽ��ͽ��ͽ��ͽ���        �L�ͽ���    ���ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ���        ���ͽ���        �L�ͽ��;L�;L�;L�ͽ��ͽ��ͽ��;L��                    ����    ����    ���ͽ��ͽ��;������ͽ��ͽ��ͽ���=��ͽ���    �L�;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ���    ���ͽ���    ���;L��    ���;L�ͽ��;L�;L�;L��    ����    =���=���>L��>L��>���>L��>���>���?��?   ?   ?��?333?333?L��?fff?�  ?�  ?�  ?���?���?���?�ff?�33?�33?�  ?�  ?���?ٙ�?ٙ�?ٙ�?�ff?�33@   @ff@��@��@33@��@   @&ff@,��@333@9��@@  @L��@S33@Y��@`  @fff@s33@y��@�  @�33@���@���@�  @�33@���@���@�  @�33@�ff@���@�  @�  @�33@���@�  @�33@�ff@ə�@���@�  @�ff@ٙ�@���@�33@�ff@���@�  @�33@���@���A   A33A��AffA	��A33A��A  A��A33AffA  A��A33AffA   A!��A#33A&ffA(  A)��A+33A.ffA0  A1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AD��AH  AI��AK33AL��ANffAP  AQ��AT��AVffAX  AY��A[33A^ffA`  Aa��Ac33Ad��AfffAh  Ak33Al��AnffAp  Aq��As33At��Ax  Ay��A{33A|��A~ffA���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A���Ař�A�ffA�  A���Aə�A�33A�  A͙�A�ffA�  A���Aљ�A�33A�  Aՙ�A�ffA�  A���A�ffA�33Do�Do&fDo,�Do9�DoFfDoS3Do` DoffDos3Do� Do��Do��Do� Do��Do��Do�fDo�3DoٚDo�fDo�3Dp  Dp�Dp3Dp  Dp,�Dp9�DpFfDpL�DpY�DpffDps3Dp� Dp�fDp�3Dp� Dp��Dp��Dp�fDp��DpٚDp�fDp�3Dq  DqfDq3Dq  Dq,�Dq9�DqFfDqL�DqY�DqffDqs3Dq� Dq��Dq�3Dq� Dq��Dq��Dq�fDq�3Dq� Dq�fDq�3Dr  Dr�Dr�Dr&fDr,�Dr9�DrFfDrS3Dr` Drl�Drs3Dr� Dr��Dr��Dr�fDr�3Dr� Dr�fDr�3Dr� Dr��Dr��Ds  Ds�Ds�Ds&fDs33Ds@ DsL�DsS3Ds` Dsl�Dsy�Ds�fDs�3Ds��Ds�fDs�3Ds� Ds��DsٚDs� Ds��Ds��DtfDt3Dt  Dt&fDt33Dt@ DtL�DtY�DtffDts3Dty�Dt�f@&ff@,��@333@9��@@  @L��@S33@Y��@`  @fff@s33@y��@�  @�33@���@���@�  @�33@���@���@�  @�33@�ff@���@�  @�  @�33@���@�  @�33@�ff@ə�@���@�  @�ff@ٙ�@���@�33@�ff@���@�  @�33@���@���A   A33A��AffA	��A33A��A  A��A33AffA  A��A33AffA   A!��A#33A&ffA(  A)��A+33A.ffA0  A1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AD��AH  AI��AK33AL��ANffAP  AQ��AT��AVffAX  AY��A[33A^ffA`  Aa��Ac33Ad��AfffAh  Ak33Al��AnffAp  Aq��As33At��Ax  Ay��A{33A|��A~ffA���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A���Ař�A�ffA�  A���Aə�A�33A�  A͙�A�ffA�  A���Aљ�A�33A�  Aՙ�A�ffA�  A���A�ffA�33Do�Do&fDo,�Do9�DoFfDoS3Do` DoffDos3Do� Do��Do��Do� Do��Do��Do�fDo�3DoٚDo�fDo�3Dp  Dp�Dp3Dp  Dp,�Dp9�DpFfDpL�DpY�DpffDps3Dp� Dp�fDp�3Dp� Dp��Dp��Dp�fDp��DpٚDp�fDp�3Dq  DqfDq3Dq  Dq,�Dq9�DqFfDqL�DqY�DqffDqs3Dq� Dq��Dq�3Dq� Dq��Dq��Dq�fDq�3Dq� Dq�fDq�3Dr  Dr�Dr�Dr&fDr,�Dr9�DrFfDrS3Dr` Drl�Drs3Dr� Dr��Dr��Dr�fDr�3Dr� Dr�fDr�3Dr� Dr��Dr��Ds  Ds�Ds�Ds&fDs33Ds@ DsL�DsS3Ds` Dsl�Dsy�Ds�fDs�3Ds��Ds�fDs�3Ds� Ds��DsٚDs� Ds��Ds��DtfDt3Dt  Dt&fDt33Dt@ DtL�DtY�DtffDts3Dty�Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @G
=@��@��A��A$��AD��Ad��A�z�A�z�A��A�z�A�z�A�z�A�G�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\Cvh�CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl=Dl��Dm�Dm��Dn�Dn��DoqDo��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds�=Dt�Dtz=DyqHD�4D�H D���D�� D�	�D�L)D��qD��>D�#�D�D)D���D���D���D�-�Dڀ D��D�{D�D{D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���Q�<#����Q콸Q콸Q콸Q�<#��<#��<#����Q�<#����Q�<#��<#��<#����Q�<#����Q콸Q콸Q콸Q콸Q콸Q�>��R>��>��R<#��<#��<#��<#��<#����Q콸Q�=�G�<#��<#��<#��<#��>W
=<#����Q�<#��=�G���Q�<#��<#��<#��<#����Q�<#��<#����Q콸Q�<#����Q콸Q�<#��<#����Q�<#��<#����Q�<#��<#��<#����Q�<#��<#����Q�<#��=�G�<#����Q�=�G�<#����Q�<#��<#����Q�<#��=�G���Q콸Q�<#��<#��<#����Q콸Q�<#����Q�<#����Q콸Q콸Q�<#��<#��=�G���Q콸Q�<#��=�G�<#����Q�<#��=�G�=�G�<#����Q�<#����Q콸Q�=�G�>��R=�G���Q�<#��>��R>W
=��Q�=�G���Q콸Q�<#��=�G�<#����Q콸Q�=�G�>W
=��Q�<#����Q콸Q�<#��>W
=��Q�<#����Q콸Q�<#��=�G���Q콸Q�>W
==�G���Q콸Q�<#��=�G�<#����Q콸Q콸Q�<#��=�G�>��R<#����Q콸Q콸Q콸Q�<#��>W
=<#����Q콸Q�<#��=�G�=�G���Q콸Q�=�G�=�G���Q콸Q콸Q�=�G�>W
==�G���Q콸Q�=�G�<#��=�G�>��R>W
=<#��<#��<#����Q�=�G�>W
==�G�<#��<#��<#��<#��=�G�>W
=<#����Q�<#��=�G�>W
=<#��<#��=�G�>��=�G���Q콸Q�>W
=>W
=>��R>��R>W
=��Q콸Q�<#��=�G�=�G�>W
==�G�=�G�=�G���Q�=�G�=�G�=�G�=�G�=�G�<#��>W
=>W
==�G�>��R=�G�=�G�>W
==�G�=�G�>W
=>��R=�G�>W
=>��R>W
=>W
=>W
=>W
=<#��=�G�>W
=>��R>W
=>W
==�G�=�G�>��R>W
==�G�>W
=>W
=>W
=>W
==�G�>W
=>W
=>W
=>��R>��R=�G�>W
=>W
==�G�=�G�>W
=>W
=>W
=>W
=>W
=>W
=>W
==�G�>W
=>W
=>W
=>W
==�G�>W
=>W
==�G�>��R>W
=>W
=>W
=>��R>��R>��R>��R>W
=>W
==�G�>W
==�G�>W
=>W
=>W
=<#��>W
=>W
=>��R>W
==�G�=�G�>W
=>��R>��R>W
=>��R=�G�>W
=>W
=>W
=>W
=>��R>��R>��R=�G�<#��>W
=>W
=>W
==�G�=�G�>W
=>W
=>W
=>W
=>��R>��R>W
==�G�>W
=<#��>W
=>W
=>W
=>��R>W
=>W
=>W
=>W
=>W
=>W
=>��R=�G�=�G�>W
==�G�>W
=>W
=>��>��R>W
=>W
=>W
=>��R>��>��>W
==�G�<#��=�G�>W
==�G�>��R>W
==�G�>W
=>W
=>W
=>W
==�G�=�G�=�G�>W
=>��R=�G�>W
=>W
=>W
=>W
==�G�>W
=>W
=>W
==�G�>W
=>W
=>��R>W
=>W
=>��R=�G�=�G�>W
==�G�>W
=>W
=>��R>��R>W
==�G�>W
=>W
=>W
=>W
=>W
=>��R>��R=�G�>W
=>��R>W
=>W
==�G�=�G�=�G�>W
=>W
==�G�>W
=>��R>��R>W
=>W
=>��R>��R=�G�>W
==�G�=�G�=�G�>W
=>W
=>W
==�G�>��R>��R>��R>��R>��R>W
=>��R>W
=>��R>W
=>W
=>W
=<#��>W
=>W
=>W
=>W
=>��>W
=>��R=�G�=�G�>W
=>W
=>W
=>W
==�G�>W
=>��R>W
=>W
=>��R>W
==�G�>��R>W
==�G�>W
==�G�=�G�=�G�>��R>W
=>��R>��>��?�\?�\?(�?�\?5?5?h��?O\)?O\)?h��?�G�?�G�?�{?��G?��?��?��?�z�?�G�?�G�?�z?��G?��G?�?�?�z�@ ��@ ��@ ��@
=@p�@�
@=p@ ��@ ��@'
=@-p�@3�
@:=p@@��@G
=@Mp�@S�
@`��@g
=@mp�@s�
@z=p@��@��R@��@��@��@��R@��@��@��@��R@��@��@�Q�@��R@��@��@��@Å@��@��@�Q�@Ӆ@ָR@��@�Q�@�@�R@��@�Q�@��R@��@��AA\)A��A(�A	A\)A�]A(�AA��A�]A(�A\)A��A�]A (�A#\)A$��A&�]A((�A+\)A,��A.�]A0(�A3\)A4��A6�]A8(�A;\)A<��A>�]A@(�AAAD��AF�]AH(�AIAL��AN�]AP(�AQAS\)AT��AV�]AYA[\)A\��A^�]A`(�Ac\)Ad��Af�]Ah(�AiAk\)Al��Ap(�AqAs\)At��Av�]Ax(�AyA|��A~�]A�{A��HA��A�G�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A��GA��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��GAŮA�G�A�{A��GA�z�A�G�A�{AͮA�z�A�{A��GA�z�A�G�A�{AծA�z�A�{A��GA�z�A�G�A��GAݮDo-qDo:=Do@�DoMqDoZ=Dog
Dos�Doz=Do�
Do��Do��Do�qDo��Do��Do�qDo�=Do�
Do�qDo�=Dp
Dp�Dp �Dp'
Dp3�Dp@�DpMqDpZ=Dp`�DpmqDpz=Dp�
Dp��Dp�=Dp�
Dp��Dp��Dp�qDp�=Dp�Dp�qDp�=Dq
Dq�Dq=Dq'
Dq3�Dq@�DqMqDqZ=Dq`�DqmqDqz=Dq�
Dq��Dq��Dq�
Dq��Dq��Dq�qDq�=Dq�
Dq��Dq�=Dr
Dr�Dr �Dr-qDr:=Dr@�DrMqDrZ=Drg
Drs�Dr��Dr�
Dr��Dr��Dr�qDr�=Dr�
Dr��Dr�=Dr�
Dr��Ds �DsqDs�Ds �Ds-qDs:=DsG
DsS�Ds`�Dsg
Dss�Ds��Ds�qDs�=Ds�
Ds�qDs�=Ds�
Ds��Ds�Ds�qDs��Dt �DtqDt=Dt'
Dt3�Dt:=DtG
DtS�Dt`�DtmqDtz=Dt�
Dt�qDt�=@:=p@@��@G
=@Mp�@S�
@`��@g
=@mp�@s�
@z=p@��@��R@��@��@��@��R@��@��@��@��R@��@��@�Q�@��R@��@��@��@Å@��@��@�Q�@Ӆ@ָR@��@�Q�@�@�R@��@�Q�@��R@��@��AA\)A��A(�A	A\)A�]A(�AA��A�]A(�A\)A��A�]A (�A#\)A$��A&�]A((�A+\)A,��A.�]A0(�A3\)A4��A6�]A8(�A;\)A<��A>�]A@(�AAAD��AF�]AH(�AIAL��AN�]AP(�AQAS\)AT��AV�]AYA[\)A\��A^�]A`(�Ac\)Ad��Af�]Ah(�AiAk\)Al��Ap(�AqAs\)At��Av�]Ax(�AyA|��A~�]A�{A��HA��A�G�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A��GA��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��GAŮA�G�A�{A��GA�z�A�G�A�{AͮA�z�A�{A��GA�z�A�G�A�{AծA�z�A�{A��GA�z�A�G�A��GAݮDo-qDo:=Do@�DoMqDoZ=Dog
Dos�Doz=Do�
Do��Do��Do�qDo��Do��Do�qDo�=Do�
Do�qDo�=Dp
Dp�Dp �Dp'
Dp3�Dp@�DpMqDpZ=Dp`�DpmqDpz=Dp�
Dp��Dp�=Dp�
Dp��Dp��Dp�qDp�=Dp�Dp�qDp�=Dq
Dq�Dq=Dq'
Dq3�Dq@�DqMqDqZ=Dq`�DqmqDqz=Dq�
Dq��Dq��Dq�
Dq��Dq��Dq�qDq�=Dq�
Dq��Dq�=Dr
Dr�Dr �Dr-qDr:=Dr@�DrMqDrZ=Drg
Drs�Dr��Dr�
Dr��Dr��Dr�qDr�=Dr�
Dr��Dr�=Dr�
Dr��Ds �DsqDs�Ds �Ds-qDs:=DsG
DsS�Ds`�Dsg
Dss�Ds��Ds�qDs�=Ds�
Ds�qDs�=Ds�
Ds��Ds�Ds�qDs��Dt �DtqDt=Dt'
Dt3�Dt:=DtG
DtS�Dt`�DtmqDtz=Dt�
Dt�qDt�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A�/A�/A�33A�33A�33A�33A�-A�(�A�(�A� �A��A�oA�bA��A�"�A�A���A�A��^A��^A��^A��^A��RA��FA��9A��-A��!A��!A��!A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��+A��A��A��A��A��A�~�A�t�A�5?A�JA��RA��A�jA�ĜA�VA�ȴA�n�A�+A�/A�^5A��A��!A�S�A��`A��A��uA�^5A� �A���A���A���A�dZA�"�A��A���A�v�A�G�A�+A��TA���A�$�A�E�A���A���A�bA�1'A�bNA�p�A�A�+A���A�A�|�A��TA� �A��yA�r�A��A�~�A�ĜA��RA���A��A�{A�E�A�`BA��mA��uA�{A�ffA���A�M�A��9A�1'A���A�(�A���A�7LAx�yAvbNAu|�At�9AsK�Ao��Aj{Ah�Ae�hAdffAbQ�A_�TA]��A["�AX��AX(�AV�!AU�7AT�`ASC�ARE�AQS�AP�AMp�AJ��AFĜAE��AD��ABĜA?�A=VA:��A8��A7�A6��A6�A5%A3&�A2$�A0ĜA/dZA.�RA-��A-l�A+oA*1A*  A)�#A)`BA(�A'��A'"�A&�A&(�A%��A#��A#�A"1'A!�A (�A`BA�DA�FAbAZA33AĜA�DA1A��A�yA1'A��A�AXAC�A��AhsAbNAƨA��A��A��A�RA�!A�A�A
I�A	��A��A33AAO�A�yAI�A��A�A �`A ��@�@�/@�33@���@�A�@��@��+@�x�@�Q�@�~�@�@�;d@�
=@�+@�^5@���@���@�bN@�K�@�9@�t�@���@�v�@�@��@�Ĝ@�r�@��
@��@��`@���@�$�@��@�@���@�hs@���@���@ۍP@�Z@���@�p�@Դ9@�b@��@ҧ�@���@�O�@�j@ϝ�@�l�@�
=@θR@Χ�@Χ�@�
=@��@��@�  @˶F@� �@�Z@�"�@ə�@��@�+@�&�@�Ĝ@Ĵ9@�l�@\@��`@��@��@�X@�`B@���@�j@�I�@��@��@��\@��\@�M�@�=q@�M�@�=q@��T@�7L@���@��u@�Z@�9X@��@��m@��@�"�@�K�@�S�@�33@��\@��@��@�V@��u@��m@��P@�@��!@�dZ@�dZ@��j@�v�@��@��u@���@�A�@�I�@�z�@�bN@�9X@�(�@�  @�ƨ@�b@�1'@�ƨ@��@��H@���@��+@���@�-@��@�7L@��/@�Z@��@��F@�;d@���@�^5@���@�`B@�%@��@��u@��D@�z�@��@��F@���@�t�@�o@�5?@�@���@�x�@��`@���@�@��H@�ff@��-@��T@��@��
@��F@���@�|�@�t�@�C�@�ȴ@�~�@��\@�V@��@��-@�hs@�/@��`@��@�7L@�G�@��@�bN@���@��@���@��-@��@�j@��w@�\)@�"�@��@��R@���@�ff@��@�x�@���@��D@��D@�j@�I�@�9X@��@���@���@��@���@��-@���@��@�O�@�Ĝ@���@��@�(�@��;@��@�C�@�+@��@��H@��R@��\@�ff@���@�@��P@��@��@�1@�dZ@���@�J@�x�@���@�j@�1'@�(�@� �@��@�b@�1'@��@��D@�A�@���@��@�+@�ȴ@���@�n�@�V@���@��#@�O�@�V@��@���@�z�@��m@�S�@���@�@� i@x~@q7L@f@]�z@Tی@NM�@F��@>}V@7�0@3��@-�@)�@"�x@T�@1�@�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�  A��`A�%A���A�bNA���A�O�A��A�oA���A��+A��DA��mA�A�A��mA�I�A��#A�C�A�~�A��A�n�A���A��A��A�\)A���A��A��A�M�A��A�v�A��A��A��PA��A�
=A��A��A�dZA���A��A�r�A���A�JA��7A�A���A�l�A��A�M�A�`BA��A��A�7LA��!A��A�  A� �A��^A�9XA�5?A��^A�r�A���A�t�A� �A�$�A��A��jA��A��A��FA��A�p�A��HA���A�7LA��jA��#A���A��/A�bA���A�33A�r�A���A�ȴA�(�A�7LA�n�A�bA�A�33A��hA���A�G�A�XA�%A� �A��A��DA��A��/A�"�A��A�dZA��A��`A�%A��A� �A� �A��hA���A��A� �A�jA�1'A��A��A���A�E�A�hsA�A�ƨA�Q�A��A���A�hsA�VA�bNA��^A� �A��jA�E�A��A��A��A��A�VA�v�A�JA��A�t�A�33A�5?A��
A� �A���A��HA���A���A�1'A�"�A��A��hA��yA��A�1A�l�A� �A��A���A�|�A�A���A� �A�`BA��jA�r�A�"�A��A��9A���A��A� �A��A��A�;dA��PA� �A��hA�$�A�"�A�A�VA���A��A��A�"�A� �A�A���A�/A�9XA� �A��A��wA��A�$�A���A�"�A�ffA�n�A� �A�"�A�"�A��A���A���A�"�A�"�A�"�A�"�A���A�+A�ƨA�$�A�$�A�"�A�$�A�"�A�ffA�+A��yA�"�A� �A���A� �A��A� �A� �A�"�A�"�A� �A��A��A�"�A��A�"�A�$�A�"�A�"�A��A� �A�"�A�$�A� �A�"�A�ffA�"�A�"�A�$�A�"�A�$�A�$�A�$�A�$�A�"�A�$�A�"�A�$�A� �A�"�A�"�A�"�A�$�A�"�A�"�A�"�A�"�A� �A�"�A�"�A�$�A�"�A�"�A� �A� �A� �A�"�A� �A��A� �A� �A� �A��A��A� �A� �A� �A�"�A��A� �A�"�A�$�A�"�A�$�A� �A�"�A�$�A�&�A�&�A�$�A�$�A�"�A���A�$�A�$�A�$�A�"�A�$�A�ȴA�"�A�$�A�&�A�$�A�$�A�"�A� �A� �A�$�A� �A�"�A�$�A�&�A�"�A��A��/A� �A�$�A� �A�$�A�"�A� �A��A��A� �A�$�A��A��A��A��A�&�A�&�A� �A�"�A�&�A�(�A�(�A�"�A�$�A�&�A�(�A�&�A�$�A�&�A�bA�&�A�+A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�&�A�(�A�A��A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�$�A���A�&�A�(�A��A�&�A�&�A�(�A�&�A�"�A�&�A�&�A�&�A�"�A�$�A�$�A�(�A�(�A�(�A�(�A�&�A�$�A�&�A�&�A�&�A�&�A�$�A�&�A�$�A�$�A�&�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�&�A�$�A�$�A�"�A�$�A�$�A� �A�$�A�&�A�$�A�&�A�&�A�&�A�$�A�&�A�"�A�$�A�$�A�"�A�"�A�$�A�$�A�&�A�&�A�&�A�$�A�&�A�$�A�$�A�$�A�"�A�$�A� �A�$�A�&�A�"�A�"�A� �A���A��A�$�A�&�A�&�A�&�A�&�A�$�A�$�A���A�&�A�(�A�$�A�$�A� �A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�$�A�&�A�"�A�"�A�$�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�(�A�&�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�$�A�&�A�+A�+A�+A�+A�-A�-A�-A�+A�-A�-A�-A�-A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�-A�/A�/A�/A�/A�1'A�-A�/A�/A�/A�-A�/A�-A�-A�+A�-A�-A�-A�-A�/A�1'A�/A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�1'A�1'A�33A�1'A�33A�33A�33A�33A�5?A�33A�33A�33A�33A�33A�5?A�5?A�1'A�1'A�33A�1'A�1'A�/A�1'A�33A�/A�1'A�1'A�33A�1'A�33A�1'A�1'A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�33A�33A�1'A�1'A�1'A�1'A�33A�1'A�1'A�33A�33A�33A�1'A�1'A�1'A�33A�1'A�33A�1'A�33A�33A�1'A�1'A�33A�1'A�1'A�-A�+A�(�A�(�A�+A�(�A�(�A�+A�(�A�+A�+A�(�A�(�A�-A�-A�-A�+A�&�A�$�A�"�A�&�A�&�A�&�A�(�A�(�A�&�A�$�A�&�A�&�A�&�A�(�A�(�A�+A�(�A�+A�-A�+A�(�A�(�A�(�A�+A�(�A�&�A�$�A�$�A��A� �A� �A��A��A��A��A��A��A��A��A�$�A��A��A��A��A��A��A� �A��A�JA�bA�VA�oA�{A�bA�bA�{A�bA�bA�VA�VA�oA�oA�{A��A��A��A��A�
=A�oA�b@��\@��\@��+@��+@�~�@�~�@�v�@�v�@�v�@�n�@�n�@�n�@�ff@�n�@�ff@�^5@�^5@�ff@�^5@�^5@�^5@�V@�V@�M�@�M�@�5?@�@��@��#@��-@��h@��7@�p�@�`B@�X@�hs@�`B@��#@��#@��T@��@��@��T@��@��@��#@���@���@�x�@�hs@�`B@�X@�G�@�G�@�?}@�?}@�?}@�?}@�?}@�?}@�?}@�7L@��@�%@���@���@���@��`@���@���@��j@��9@���@���@��u@���@��@��9@��9@��9@��j@��j@�Ĝ@��j@��@��@��@���@��D@��D@��D@��D@��@��@��D@��@��D@��u@�z�@�bN@�Z@�Q�@�I�@�9X@�1'@��@�b@�  @��;@�ƨ@��@��P@�t�@�t�@�l�@�dZ@�S�@�C�@�;d@�
=A�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�/A�/A�/A�/A�1'A�/A�/A�/A�-A�-A�-A�-A�-A�-A�/A�/A�/A�1'A�1'A�1'A�/A�1'A�/A�1'A�1'A�33A�1'A�1'A�1'A�33A�33A�33A�1'A�33A�1'A�1'A�33A�33A�33A�33A�33A�33A�33A�5?A�5?A�5?A�5?A�33A�1'A�33A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�1'A�33A�1'A�33A�33A�33A�33A�33A�33A�33A�33A�33A�1'A�33A�33A�5?A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�1'A�1'A�1'A�1'A�1'A�1'A�+A�&�A�(�A�+A�&�A�&�A�&�A�+A�-A�+A�+A�+A�-A�+A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�&�A�+A�(�A�+A�$�A�&�A�&�A�(�A�(�A�+A�+A�(�A�+A�+A�(�A�(�A�(�A�+A�(�A�&�A�$�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A��A�{A�VA�VA�bA�oA�bA�VA�oA�oA�bA�VA�VA�VA�bA�{A��A��A��A��A�VA��A�o@���@��\@��+@��+@�~�@�~�@�v�@�v�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�ff@�^5@�V@�^5@�V@�V@�V@�=q@�5?@�{@��@��#@�@��h@��7@�x�@�hs@�X@�hs@�`B@�@��T@��T@��@��@��@��@��@��T@���@���@��@�hs@�hs@�X@�O�@�G�@�?}@�?}@�?}@�?}@�G�@�?}@�?}@�?}@��@�%@���@���@���@��`@��/@���@�Ĝ@��9@��@���@��u@��u@���@��9@��9@��9@��j@�Ĝ@�Ĝ@��j@��9@��9@��@���@��u@��D@��D@��D@��D@��@��D@��D@��D@��u@��@�bN@�Z@�Q�@�I�@�A�@�1'@� �@�b@�1@��m@�ƨ@��@��P@�|�@�t�@�l�@�dZ@�\)@�K�@�;d@�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�-A�/A�/A�33A�33A�33A�33A�-A�(�A�(�A� �A��A�oA�bA��A�"�A�A���A�A��^A��^A��^A��^A��RA��FA��9A��-A��!A��!A��!A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��+A��A��A��A��A��A�~�A�t�A�5?A�JA��RA��A�jA�ĜA�VA�ȴA�n�A�+A�/A�^5A��A��!A�S�A��`A��A��uA�^5A� �A���A���A���A�dZA�"�A��A���A�v�A�G�A�+A��TA���A�$�A�E�A���A���A�bA�1'A�bNA�p�A�A�+A���A�A�|�A��TA� �A��yA�r�A��A�~�A�ĜA��RA���A��A�{A�E�A�`BA��mA��uA�{A�ffA���A�M�A��9A�1'A���A�(�A���A�7LAx�yAvbNAu|�At�9AsK�Ao��Aj{Ah�Ae�hAdffAbQ�A_�TA]��A["�AX��AX(�AV�!AU�7AT�`ASC�ARE�AQS�AP�AMp�AJ��AFĜAE��AD��ABĜA?�A=VA:��A8��A7�A6��A6�A5%A3&�A2$�A0ĜA/dZA.�RA-��A-l�A+oA*1A*  A)�#A)`BA(�A'��A'"�A&�A&(�A%��A#��A#�A"1'A!�A (�A`BA�DA�FAbAZA33AĜA�DA1A��A�yA1'A��A�AXAC�A��AhsAbNAƨA��A��A��A�RA�!A�A�A
I�A	��A��A33AAO�A�yAI�A��A�A �`A ��@�@�/@�33@���@�A�@��@��+@�x�@�Q�@�~�@�@�;d@�
=@�+@�^5@���@���@�bN@�K�@�9@�t�@���@�v�@�@��@�Ĝ@�r�@��
@��@��`@���@�$�@��@�@���@�hs@���@���@ۍP@�Z@���@�p�@Դ9@�b@��@ҧ�@���@�O�@�j@ϝ�@�l�@�
=@θR@Χ�@Χ�@�
=@��@��@�  @˶F@� �@�Z@�"�@ə�@��@�+@�&�@�Ĝ@Ĵ9@�l�@\@��`@��@��@�X@�`B@���@�j@�I�@��@��@��\@��\@�M�@�=q@�M�@�=q@��T@�7L@���@��u@�Z@�9X@��@��m@��@�"�@�K�@�S�@�33@��\@��@��@�V@��u@��m@��P@�@��!@�dZ@�dZ@��j@�v�@��@��u@���@�A�@�I�@�z�@�bN@�9X@�(�@�  @�ƨ@�b@�1'@�ƨ@��@��H@���@��+@���@�-@��@�7L@��/@�Z@��@��F@�;d@���@�^5@���@�`B@�%@��@��u@��D@�z�@��@��F@���@�t�@�o@�5?@�@���@�x�@��`@���@�@��H@�ff@��-@��T@��@��
@��F@���@�|�@�t�@�C�@�ȴ@�~�@��\@�V@��@��-@�hs@�/@��`@��@�7L@�G�@��@�bN@���@��@���@��-@��@�j@��w@�\)@�"�@��@��R@���@�ff@��@�x�@���@��D@��D@�j@�I�@�9X@��@���@���@��@���@��-@���@��@�O�@�Ĝ@���@��@�(�@��;@��@�C�@�+@��@��H@��R@��\@�ff@���@�@��P@��@��@�1@�dZ@���@�J@�x�@���@�j@�1'@�(�@� �@��@�b@�1'@��@��D@�A�@���@��@�+@�ȴ@���@�n�@�V@���@��#@�O�@�V@��@���@�z�@��mG�O�@���@�@� i@x~@q7L@f@]�z@Tی@NM�@F��@>}V@7�0@3��@-�@)�@"�x@T�@1�@�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�  A��`A�%A���A�bNA���A�O�A��A�oA���A��+A��DA��mA�A�A��mA�I�A��#A�C�A�~�A��A�n�A���A��A��A�\)A���A��A��A�M�A��A�v�A��A��A��PA��A�
=A��A��A�dZA���A��A�r�A���A�JA��7A�A���A�l�A��A�M�A�`BA��A��A�7LA��!A��A�  A� �A��^A�9XA�5?A��^A�r�A���A�t�A� �A�$�A��A��jA��A��A��FA��A�p�A��HA���A�7LA��jA��#A���A��/A�bA���A�33A�r�A���A�ȴA�(�A�7LA�n�A�bA�A�33A��hA���A�G�A�XA�%A� �A��A��DA��A��/A�"�A��A�dZA��A��`A�%A��A� �A� �A��hA���A��A� �A�jA�1'A��A��A���A�E�A�hsA�A�ƨA�Q�A��A���A�hsA�VA�bNA��^A� �A��jA�E�A��A��A��A��A�VA�v�A�JA��A�t�A�33A�5?A��
A� �A���A��HA���A���A�1'A�"�A��A��hA��yA��A�1A�l�A� �A��A���A�|�A�A���A� �A�`BA��jA�r�A�"�A��A��9A���A��A� �A��A��A�;dA��PA� �A��hA�$�A�"�A�A�VA���A��A��A�"�A� �A�A���A�/A�9XA� �A��A��wA��A�$�A���A�"�A�ffA�n�A� �A�"�A�"�A��A���A���A�"�A�"�A�"�A�"�A���A�+A�ƨA�$�A�$�A�"�A�$�A�"�A�ffA�+A��yA�"�A� �A���A� �A��A� �A� �A�"�A�"�A� �A��A��A�"�A��A�"�A�$�A�"�A�"�A��A� �A�"�A�$�A� �A�"�A�ffA�"�A�"�A�$�A�"�A�$�A�$�A�$�A�$�A�"�A�$�A�"�A�$�A� �A�"�A�"�A�"�A�$�A�"�A�"�A�"�A�"�A� �A�"�A�"�A�$�A�"�A�"�A� �A� �A� �A�"�A� �A��A� �A� �A� �A��A��A� �A� �A� �A�"�A��A� �A�"�A�$�A�"�A�$�A� �A�"�A�$�A�&�A�&�A�$�A�$�A�"�A���A�$�A�$�A�$�A�"�A�$�A�ȴA�"�A�$�A�&�A�$�A�$�A�"�A� �A� �A�$�A� �A�"�A�$�A�&�A�"�A��A��/A� �A�$�A� �A�$�A�"�A� �A��A��A� �A�$�A��A��A��A��A�&�A�&�A� �A�"�A�&�A�(�A�(�A�"�A�$�A�&�A�(�A�&�A�$�A�&�A�bA�&�A�+A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�&�A�(�A�A��A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�$�A���A�&�A�(�A��A�&�A�&�A�(�A�&�A�"�A�&�A�&�A�&�A�"�A�$�A�$�A�(�A�(�A�(�A�(�A�&�A�$�A�&�A�&�A�&�A�&�A�$�A�&�A�$�A�$�A�&�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�&�A�$�A�$�A�"�A�$�A�$�A� �A�$�A�&�A�$�A�&�A�&�A�&�A�$�A�&�A�"�A�$�A�$�A�"�A�"�A�$�A�$�A�&�A�&�A�&�A�$�A�&�A�$�A�$�A�$�A�"�A�$�A� �A�$�A�&�A�"�A�"�A� �A���A��A�$�A�&�A�&�A�&�A�&�A�$�A�$�A���A�&�A�(�A�$�A�$�A� �A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�$�A�&�A�"�A�"�A�$�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�(�A�&�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�$�A�&�A�+A�+A�+A�+A�-A�-A�-A�+A�-A�-A�-A�-A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�/A�/A�/A�/A�1'A�/A�/A�/A�-A�-A�-A�-A�-A�-A�/A�/A�/A�1'A�1'A�1'A�/A�1'A�/A�1'A�1'A�33A�1'A�1'A�1'A�33A�33A�33A�1'A�33A�1'A�1'A�33A�33A�33A�33A�33A�33A�33A�5?A�5?A�5?A�5?A�33A�1'A�33A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�1'A�33A�1'A�33A�33A�33A�33A�33A�33A�33A�33A�33A�1'A�33A�33A�5?A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�1'A�1'A�1'A�1'A�1'A�1'A�+A�&�A�(�A�+A�&�A�&�A�&�A�+A�-A�+A�+A�+A�-A�+A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�&�A�+A�(�A�+A�$�A�&�A�&�A�(�A�(�A�+A�+A�(�A�+A�+A�(�A�(�A�(�A�+A�(�A�&�A�$�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A��A�{A�VA�VA�bA�oA�bA�VA�oA�oA�bA�VA�VA�VA�bA�{A��A��A��A��A�VA��A�o@���@��\@��+@��+@�~�@�~�@�v�@�v�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�ff@�^5@�V@�^5@�V@�V@�V@�=q@�5?@�{@��@��#@�@��h@��7@�x�@�hs@�X@�hs@�`B@�@��T@��T@��@��@��@��@��@��T@���@���@��@�hs@�hs@�X@�O�@�G�@�?}@�?}@�?}@�?}@�G�@�?}@�?}@�?}@��@�%@���@���@���@��`@��/@���@�Ĝ@��9@��@���@��u@��u@���@��9@��9@��9@��j@�Ĝ@�Ĝ@��j@��9@��9@��@���@��u@��D@��D@��D@��D@��@��D@��D@��D@��u@��@�bN@�Z@�Q�@�I�@�A�@�1'@� �@�b@�1@��m@�ƨ@��@��P@�|�@�t�@�l�@�dZ@�\)@�K�@�;d@�oA�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�/A�/A�/A�/A�1'A�/A�/A�/A�-A�-A�-A�-A�-A�-A�/A�/A�/A�1'A�1'A�1'A�/A�1'A�/A�1'A�1'A�33A�1'A�1'A�1'A�33A�33A�33A�1'A�33A�1'A�1'A�33A�33A�33A�33A�33A�33A�33A�5?A�5?A�5?A�5?A�33A�1'A�33A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�1'A�33A�1'A�33A�33A�33A�33A�33A�33A�33A�33A�33A�1'A�33A�33A�5?A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�1'A�33A�33A�33A�1'A�1'A�1'A�1'A�1'A�1'A�+A�&�A�(�A�+A�&�A�&�A�&�A�+A�-A�+A�+A�+A�-A�+A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�&�A�+A�(�A�+A�$�A�&�A�&�A�(�A�(�A�+A�+A�(�A�+A�+A�(�A�(�A�(�A�+A�(�A�&�A�$�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A��A�{A�VA�VA�bA�oA�bA�VA�oA�oA�bA�VA�VA�VA�bA�{A��A��A��A��A�VA��A�o@���@��\@��+@��+@�~�@�~�@�v�@�v�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�ff@�^5@�V@�^5@�V@�V@�V@�=q@�5?@�{@��@��#@�@��h@��7@�x�@�hs@�X@�hs@�`B@�@��T@��T@��@��@��@��@��@��T@���@���@��@�hs@�hs@�X@�O�@�G�@�?}@�?}@�?}@�?}@�G�@�?}@�?}@�?}@��@�%@���@���@���@��`@��/@���@�Ĝ@��9@��@���@��u@��u@���@��9@��9@��9@��j@�Ĝ@�Ĝ@��j@��9@��9@��@���@��u@��D@��D@��D@��D@��@��D@��D@��D@��u@��@�bN@�Z@�Q�@�I�@�A�@�1'@� �@�b@�1@��m@�ƨ@��@��P@�|�@�t�@�l�@�dZ@�\)@�K�@�;d@�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>3�u@�M�=�.�=�%�=Թ�>�Ex@�Q�=�v!>|>��@�Q�>��@0?�=�dE>�<@==��4=��q=�tT>��.=��T>+?��@�PH@�H�@	>p�@�	�=�ѷ=���@.�>-Ec?�do@�QY@[;d=���>�Z>��	@�Q>�n?���@�b�>��>F@9[W=�a�@���=��1>8�@�K�=1;�=3��=J��=�� =���=�y>��>�+�> �y@e�x@�>��Z?��@�}V=��>5f�@�QY@y�'>#�@���@�R >��?�^_@�Q>�w2>^@��R=�r> 4n@M.I@�do=��%>�z%=ud0=�l�=��=���@�@B8�>&�@�P�?���=؃�>e�>�Q?ɷ@�S�=�i>�m�@�QY@�RT=ʛ�>H?��}@�Se?���=��j>�@��>�>���@�R�@�Se>c�>3�0@�QY@�RT@�C>][�@�SP=��m=��@)L=��f=�'�=���>���@�Se@�S>Gj@�S&>	V�?�$@�RT@�S�>�q@�RT=Ѭ�>A�@�R�@�RT>�>@n!�@�Q�>�Se=��?��?��2@�S�>�0?��n=��?�Wi>��@�TL@�U=��=��#=��=ٮ�>=�A@�N<@�S�=���>+��=��U>��X@�SP@��)> ѷ?01�@�T@�SP=���=��}@8��@�T@�T�?tcI>�Z�>Us�@�T?��m@�T@�U\?�WT=�^>w�>���@E3�@�TL@�S�@�O�>Qhs@<��?1x-@�S�@�R�@�SP>5�">T��@C�@�U@�T�>(1'@�T�@�U@�U�>�C�>v`@7_�@�V@�U\@�U@�I�@�?_�*?�)�@�U�@�V�@�U�@�U�@�U�@v0?�k<?�w@�U\@�U\@4��@�TL@�U@��G@�U@�U@�T�@�S�@�TL@�U@�U�@�S�@�U�@�V@�U@�U\@�Ta@�T�@�Ta@�Uq@�Uq@�U�?��@�U�@�Uq@�Uq@�U�@�U\@�V@�U\@�U�@�V@�U�@�U\@�U\@�U�@�U@�U\@�U�@�U�@�Uq@�U@�U�@�U@�U\@�U\@�U\@�U\@�V@�U\@�T�@�T@�T�@�U@�TL@�S�@�U@�U@�T�@�Se@�Ta@�T�@�U@�T�@�T@�SP@�U�@�U�@�W@�VX@�V@�V@�VX@�V�@�V�@�V�@�V�@�V�@�U�@���@�V�@�V�@�V�@�V@�U�@���@�V�@�VX@�W@�V�@�V�@�U�@�U@�U@�U�@�U@�V@�VX@�W*@�V�@�T�@V�@�U\@�U\@�T�@�U�@�T@�TL@�T�@�T�@�U@�T�@�T@�T�@�T�@�V�@�W@�W*@�V�@�W*@�W*@�W~@�W�@�Wi@�W�@�W�@�W�@�V�@�W@�V�@�VX@�W�@�X@�X@�X@�W�@�W�@�W~@�W�@�X%@�W�@�X@�W�@�Wi@�N�@�X@�Wi@�W�@�X@�X%@�W�@�W�@�W�@�W~@�W�@�W�@�F�@�X@�X@�W�@�W~@�W�@�W*@�W*@�W@�W@�V�@�W@�VX@�W@�W�@�X@�W�@�X@�Wi@�W@�W~@�W~@�W~@�W~@�W~@�W*@�W�@�Wi@�Wi@�W�@�W�@�W�@�X@�W�@�Wi@�Wi@�Wi@�W�@�W�@�V�@�V�@�V�@�V@�V@�V�@�W@�V�@�Wi@�Wi@�V�@�V�@�W@�W@�W@�V�@�U�@�V�@�V�@�W~@�W�@�W~@�Wi@�W@�V�@�Wi@�W@�W@�W@�Wi@�U�@�W@�W@�W@�U�@�U�@�Ta@�V@�V�@�Wi@�W�@�W�@�Wi@�V�@�W@^�@�Wi@�W@�Wi@�Wi@�W@�W~@�W�@�W�@�Wi@�W�@�Wi@�W�@�W�@�W�@�W�@�W@�W@�V�@�W@�W*@�W*@�W~@�W�@�X%@�X@�W�@�Xd@�Xd@�Wi@�Wi@�Wi@�Wi@�Wi@�W�@�W�@�W�@�W�@�W�@�X%@�Y�@�Y�@�Y�@�Z@�Z@�Z@�Z@�Z@�Z@�Zq@�Zq@�Zq@�Zq@�Zq@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�[@�[@�[@�[@�[@�[@�[�@�[�@�[�@�[�@�[�@�[�@�\)@�[�@�\)@�\)@�\)@�[�@�[�@�[�@�[�@�\)@�\)@�\}@�\}@�\�@�]%@�]%@�]�@�]O@�]�@�]�@�^_@�^5@�^5@�^�@�^�@�^J@�^�@�^�@�^�@�^�@�^�@�^�@�^�@�^�@�_1@�_1@�^�@�_�@�_�@�_�@�_�@�_�@�`B@�`B@�`B@�`B@�`@�_F@�_�@�_�@�_p@�_�@�_�@�`@�_�@�_�@�`B@�`B@�_�@�_�@�`�@�`B@�`�@�`�@�`�@�`�@�`�@�`�@�`�@�`�@�`�@�`�@�`�@�`�@�a(@�`�@�`�@�`�@�`�@�a(@�aR@�a|@�aR@�aR@�`�@�aR@�a�@�ag@�a�@�ag@�a�@�ag@�ag@�a(@�ag@�a(@�`�@�`W@�_[@�_@�_�@�_[@�_�@�_[@�_[@�`@�`@�`�@�`@�`@�`k@�`k@�`@�`@�_1@�_@�^�@�_@�_[@�`B@�_�@�_�@�_[@�_�@�_[@�_�@�_�@�`k@�`B@�`k@�`k@�`�@�`�@�`�@�`k@�`k@�`k@�`k@�`�@�`@�^�@�^�@�^@�]d@�]d@�]@�\�@�\�@�\@�\@�[�@�\@�\}@�\h@�\�@�[l@�[�@�[�@�\h@�]�@�^ @�]@�Z@�Xd@�Y`@�Y�@�Z2@�Z@�Y`@�Y�@�Y�@�Y�@�Y�@�Z@�Z@�[@�[l@�]y@�]�@�]y@�]y@�Zq@�[l@�[�@�[@R}V@R|�@R|@R{�@Rz�@Rz@Ryh@Ry@Rx�@Rx@Rwp@Rwp@Rv�@Rv�@Ru�@Ruy@Ru%@Rt�@Rs�@RsX@Rs.@Rqa@Rpe@Rn�@Rk'@Rf<@R`�@R]�@RX�@RQ�@RQ@ROv@RM+@RL0@RM�@RQ@RZ�@ReA@Rd�@Re�@Rf�@Rf�@Rf�@Rd�@Rb�@R`W@R[@RT�@RPr@ROL@RM@RK�@RJ�@RJ@RI�@RI=@RI�@RI�@RH�@RHk@RG�@RDR@R@�@R=�@R=@R=�@R<!@R:~@R8�@R6�@R4�@R3r@R2M@R1�@R2M@R4D@R6�@R6�@R77@R8�@R9�@R9�@R9.@R7�@R77@R6�@R5@R3@R1�@R1�@R1Q@R1'@R0�@R1Q@R0�@R0�@R0@R/�@R+@R(�@R'(@R%@R#@R!@R�@R3@R�@RQ@Rg@Rx@R5@R�@RP@R T@Q��@Q��@Q��@Q�Q@Q�@Q��@��!@��!@��u@��u@���@���@���@���@���@���@��1@��1@���@���@���@���@���@��[@��[@��1@��@��1@��1@���@���@���@��W@���@���@���@���@���@���@��@���@��}@���@��}@���@���@���@���@��@��@��N@��@��9@���@���@���@��@�� @���@���@���@���@���@���@��c@���@��x@���@���@���@�� @��N@��_@��J@��_@��t@���@��_@��_@���@���@���@���@���@���@��@��1@��@��p@��@���@���@��[@��p@���@���@��p@��[@��p@��p@��p@��p@��p@���@���@���@���@���@��A@��A@���@���@���@���@���@���@��_@���@���@���@��}@���@��}@���@��5@���@��N@��@��J@��@���@��}@��)@���@���@��>@��N@��9@���@���@��t@��h@���@��N@���@���@���@���@���@���@���@��J@���@���@���@��5@��5@��}@��)@���@��@��W@��@��[@��`@���@��F@���@��!@���@���@��@��!@���@��%@��!@���@���@��@���@��2@���@��C@���@��?@���@���@���@��?@��m@���@��@�� @���@���@���@���@���@���@��p@���@R�5@R��@R��@R�@R��@R�@R��@R�F@R��@R�!@R��@R��@R��@R��@R��@R� @R��@R� @R�X@R��@R��@R��@R�`@R�@R�D@R��@R��@R�V@R�B@R��@R��@R��@R��@R�@R}�@R��@R��@R��@R��@R�N@R�k@R��@R��@R��@R�k@R�s@R��@R�p@R�;@R�Q@R�+@R�^@R��@R�E@R�@R��@R��@R�@R��@R�@R��@R��@R{�@Rv!@Rt*@Rt*@Ru�@Rs.@Rq�@Rp@RmH@Rj�@Rh�@Rg�@Rf�@Rf�@Ri�@Rl�@Rlv@Rm@Rn�@Rp;@Rp�@Rp�@Rn�@Rnn@RnD@Rl�@Rj+@Rh�@Rh�@Rh�@Rh�@Rh�@Ri�@Ri/@Ri�@Rj�@RjU@Rd@RbN@R`�@R_1@R]:@R[B@RX%@RU\@RR�@RN'@RH�@RD(@R>�@R:�@R9X@R82@R6�@R4�@R2�@R/�@R)�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               43444434443444444444444334434444433444344344443443444444444344434433433443443443344444444434444434433444344444433443334344444443343443343443343344443444443344444334444334433444334443433444443334443334443343334443333444333333443343333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�M�G�O�G�O�G�O�G�O�@�Q�G�O�G�O�G�O�@�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�PM@�H�G�O�G�O�@�	�G�O�G�O�G�O�G�O�G�O�@�Q[@[;cG�O�G�O�G�O�@�QG�O�G�O�@�b�G�O�G�O�G�O�G�O�@���G�O�G�O�@�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@e�}G�O�G�O�G�O�@�}YG�O�G�O�@�QY@y�+G�O�@���@�RG�O�G�O�@�QG�O�G�O�@��SG�O�G�O�@M.K@�dsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�P�G�O�G�O�G�O�G�O�G�O�@�S�G�O�G�O�@�QY@�RTG�O�G�O�G�O�@�SiG�O�G�O�G�O�G�O�G�O�G�O�@�R�@�SiG�O�G�O�@�Q[@�RY@�DG�O�@�SOG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Sf@�SG�O�@�S'G�O�G�O�@�RR@�S�G�O�@�ROG�O�G�O�@�R�@�RTG�O�@n!�@�Q�G�O�G�O�G�O�G�O�@�S�G�O�G�O�G�O�G�O�G�O�@�TO@�UG�O�G�O�G�O�G�O�G�O�@�NA@�S�G�O�G�O�G�O�G�O�@�SR@��*G�O�G�O�@�T@�SOG�O�G�O�G�O�@�T@�T�G�O�G�O�G�O�@�TG�O�@�T
@�U^G�O�G�O�G�O�G�O�G�O�@�TO@�S�@�O�G�O�G�O�G�O�@�S�@�R�@�SQG�O�G�O�G�O�@�U
@�T�G�O�@�T�@�U@�U�G�O�G�O�G�O�@�V@�U[@�U@�I�G�O�G�O�G�O�@�U�@�V�@�U�@�U�@�U�@v0 G�O�G�O�@�U^@�U]G�O�@�TL@�U
@��J@�U@�U@�T�@�S�@�TL@�U@�U�@�S�@�U�@�V@�U@�U^@�Tb@�T�@�Tb@�Uq@�Up@�U�G�O�@�U�@�Uq@�Uq@�U�@�U^@�V@�U[@�U�@�V@�U�@�U[@�U^@�U�@�U@�U^@�U�@�U�@�Uq@�U@�U�@�U
@�UZ@�U[@�U^@�U[@�V@�U[@�T�@�T@�T�@�U@�TO@�S�@�U@�U@�T�@�S`@�Tb@�T�@�U@�T�@�T@�SP@�U�@�U�@�W@�VZ@�V@�V@�VZ@�V�@�V�@�V�@�V�@�V�@�U�@���@�V�@�V�@�V�@�V @�U�@���@�V�@�VV@�W@�V�@�V�@�U�@�U@�U@�U�@�U@�V@�VZ@�W+@�V�@�T�@V�@�UZ@�U^@�T�@�U�@�T@�TO@�T�@�T�@�U@�T�@�T@�T�@�T�@�V�@�W@�W+@�V�@�W)@�W+@�Wz@�W�@�Wh@�W�@�W�@�W�@�V�@�W@�V�@�VY@�W�@�X@�X@�X@�W�@�W�@�W~@�W�@�X(@�W�@�X@�W�@�Wd@�N�@�X@�Wh@�W�@�X@�X"@�W�@�W�@�W�@�W~@�W�@�W�@�F�@�X@�X@�W�@�W~@�W�@�W%@�W+@�W@�W@�V�@�W@�VZ@�W@�W�@�X@�W�@�X@�Wh@�W@�W�@�W~@�W~@�W~@�W~@�W(@�W�@�Wn@�Wj@�W�@�W�@�W�@�X@�W�@�Wi@�Wh@�Wh@�W�@�W�@�V�@�V�@�V�@�V@�V@�V�@�W@�V�@�Wi@�Wi@�V�@�V�@�W@�W@�W@�V�@�U�@�V�@�V�@�W�@�W�@�W~@�Wi@�W@�V�@�Wn@�W@�W@�W@�Wn@�U�@�W@�W@�W@�U�@�U�@�T^@�V@�V�@�Wi@�W�@�W�@�Wi@�V�@�W@^�@�Wi@�W@�Wj@�Wj@�W@�W�@�W�@�W�@�Wi@�W�@�Wh@�W�@�W�@�W�@�W�@�W@�W@�V�@�W@�W.@�W+@�W~@�W�@�X"@�X@�W�@�Xg@�Xd@�Wk@�Wk@�Wh@�Wj@�Wi@�W�@�W�@�W�@�W�@�W�@�X"@�Y�@�Y�@�Y�@�Z@�Z@�Z@�Z @�Z@�Z@�Zq@�Zq@�Zs@�Zs@�Zs@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�[@�[@��#@�� @��v@��v@���@���@���@���@���@���@��1@��3@���@���@���@���@���@��a@��Z@��0@��@��3@��2@���@���@���@��Y@���@��@��@���@���@���@��@���@��{@���@��~@���@���@���@���@��@��@��M@��@��8@���@���@���@��
@��@���@���@���@���@���@���@��h@���@��z@���@���@���@��"@��P@��^@��L@��b@��y@���@��b@��a@���@���@���@���@���@���@��	@��4@��@��q@��@���@���@��`@��o@���@���@��s@��^@��s@��s@��q@��q@��r@���@���@���@���@���@��@@��B@���@���@���@���@���@���@��a@���@���@���@��z@���@��~@���@��3@���@��O@��@��L@��
@���@��z@��*@���@���@��?@��Q@��:@���@���@��u@��m@���@��Q@���@���@���@���@���@���@���@��K@���@���@���@��5@��6@��}@��&@���@��@��V@��@��]@��b@���@��C@���@��@���@���@��	@��"@���@��&@��"@���@���@��@���@��5@���@��?@���@��C@���@���@���@��C@��o@���@��@���@���@���@���@���@���@���@��q@���@R�3@R��@R��@R�@R��@R�@R��@R�H@R��@R�#@R��@R��@R��@R��@R��@R��@R��@R� @R�V@R��@R��@R��@R�c@R�@R�F@R��@R��@R�S@R�C@R� @R��@R��@R��@R�@R}�@R��@R��@R��@R��@R�N@R�k@R��@R��@R��@R�k@R�v@R��@R�p@R�=@R�M@R�*@R�`@R��@R�J@R�@R��@R��@R�@R��@R�@R��@R��@R{�@Rv@Rt+@Rt(@Ru�@Rs2@Rq�@Rp@RmJ@Rj�@Rh�@Rg�@Rf�@Rf�@Ri�@Rl�@Rlz@Rm@Rn�@Rp>@Rp�@Rp�@Rn�@Rnj@RnC@Rl�@Rj0@Rh�@Rh�@Rh�@Rh�@Rh�@Ri�@Ri.@Ri�@Rj�@RjV@Rd@RbN@R`�@R_0@R];@R[B@RX%@RU^@RR�@RN*@RH�@RD&@R>�@R:�@R9X@R83@R6�@R4�@R2�@R/�@R)�@��#@�� @��v@��v@���@���@���@���@���@���@��1@��3@���@���@���@���@���@��a@��Z@��0@��@��3@��2@���@���@���@��Y@���@��@��@���@���@���@��@���@��{@���@��~@���@���@���@���@��@��@��M@��@��8@���@���@���@��
@��@���@���@���@���@���@���@��h@���@��z@���@���@���@��"@��P@��^@��L@��b@��y@���@��b@��a@���@���@���@���@���@���@��	@��4@��@��q@��@���@���@��`@��o@���@���@��s@��^@��s@��s@��q@��q@��r@���@���@���@���@���@��@@��B@���@���@���@���@���@���@��a@���@���@���@��z@���@��~@���@��3@���@��O@��@��L@��
@���@��z@��*@���@���@��?@��Q@��:@���@���@��u@��m@���@��Q@���@���@���@���@���@���@���@��K@���@���@���@��5@��6@��}@��&@���@��@��V@��@��]@��b@���@��C@���@��@���@���@��	@��"@���@��&@��"@���@���@��@���@��5@���@��?@���@��C@���@���@���@��C@��o@���@��@���@���@���@���@���@���@���@��q@���@R�3@R��@R��@R�@R��@R�@R��@R�H@R��@R�#@R��@R��@R��@R��@R��@R��@R��@R� @R�V@R��@R��@R��@R�c@R�@R�F@R��@R��@R�S@R�C@R� @R��@R��@R��@R�@R}�@R��@R��@R��@R��@R�N@R�k@R��@R��@R��@R�k@R�v@R��@R�p@R�=@R�M@R�*@R�`@R��@R�J@R�@R��@R��@R�@R��@R�@R��@R��@R{�@Rv@Rt+@Rt(@Ru�@Rs2@Rq�@Rp@RmJ@Rj�@Rh�@Rg�@Rf�@Rf�@Ri�@Rl�@Rlz@Rm@Rn�@Rp>@Rp�@Rp�@Rn�@Rnj@RnC@Rl�@Rj0@Rh�@Rh�@Rh�@Rh�@Rh�@Ri�@Ri.@Ri�@Rj�@RjV@Rd@RbN@R`�@R_0@R];@R[B@RX%@RU^@RR�@RN*@RH�@RD&@R>�@R:�@R9X@R83@R6�@R4�@R2�@R/�@R)�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               43444434443444444444444334434444433444344344443443444444444344434433433443443443344444444434444434433444344444433443334344444443343443343443343344443444443344444334444334433444334443433444443334443334443343334443333444333333443343333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9��9�9�9�89�F9��9�o9�^9��9��9��9�9�9�*9�-9�,9��9��9��9��9��9��9�+9�S9�,9��9�29��9��9�I9�J9�]9��9�9��9�9�9�;9�(9�e9�i9��9��9��9��9��9�9�D9�D9��9��9�^9�9�,9�O9�%9�[9��9�9��9�9�#9�9��9��9��9��9��9��9�9��9��9�&9��9�&9�)9�g9�e9�z9��9��9��9��9�P9�e9��9��9��9�P9��9��9��9��9��9��9��9�9�9�9�9�E9��9��9�E9�A9�9�9�9�b9��9�;9��9�/9��9�&9�9�
9��9�9��9�9��9��9�=9��9��9�]9�p9��9��9��9�I9�}9��9��9�y9��9�19�q9��9�9��9�9�9��9�9��9�9��9��9� 9��9�g9��9��9��9��9��9��9��9�9��9��9�H9��9��9�~9��9��9�9�(9��9�/9��9�;9��9�G9��9�69�D9�L9��9�9�;9��9��9��9�G9�9�U9�99�N9�9�09L�89L��9L��9L�'9L��9L�19L��9L�i9L�9L�L9L��9L�.9L�9L��9L��9L�49L��9L�79L��9L��9L��9L�9L�9L�Z9L�9L�B9L�/9L�9L�&9L��9L�9L�!9L�9L�9L�
9L�9L�9L��9L�9L� 9L�9L�X9L�9L�9L�9L� 9L�9L�Y9L�X9Lъ9L�o9Lβ9L�9Lʸ9Lʇ9L�9L�;9Lʍ9L�9Lʇ9L�:9L�;9L�H9L��9L��9L��9L��9L�9L��9L��9L�C9L�9L��9L��9L��9L�9L��9L��9L�y9L�9L��9L�"9L��9L�p9L��9L�[9L�59L��9L�?9L��9L��9L��9L��9L��9L��9L�D9L��9L��9L�c9L�V9L��9L�L9L��9L��9L��9L��9L��9L�o9L��9L��9L�=9L�#9L�R9L��9L��9L�*9L�l9L~�9L{~9LulG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bo�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bo�Bp�Bp�Bo�Bo�Bp�Bq�Bq�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bw�Bx�Bx�Bx�Bx�Bx�By�By�By�B{�B}�B~�B� B� B�B�B� B� B}�B{�By�Bu�Bq�Bm�BgmBXBF�B8RB33B6FB8RB33B5?B8RBK�B]/B^5B^5B]/BZBXBS�BP�BN�BG�B0!B33B9XB:^B8RB+B�B	7B�B�TB��B��B�B��B�bBp�B[#BT�BH�B9XB7LB/B#�BB�B��B�!B��B�oBS�B
��B
�B
�B
�fB
ɺB
��B
�JB
o�B
P�B
2-B
%�B
�B
B	��B	�LB	�B	��B	��B	y�B	L�B	@�B	(�B	�B		7B��B�B�B��B��BƨB��B�jB�FB�'B�B��B��B��B��B��B��B�\B�=B�1B�%B�B�B�B~�B{�Bx�Bv�Bv�Bv�Bv�Bu�Bs�Bu�Bw�Bw�Bw�B}�B�B�B�B�+B�%B�B�B� B~�B~�B|�By�Bu�Bq�Bk�BgmBe`BcTBaHB^5BXBVBS�BR�BQ�BQ�BP�BN�BJ�BH�BF�BD�BB�BC�BC�BB�BD�BF�BE�BA�B>wBF�BO�BQ�BR�BS�BQ�BO�BM�BK�BH�BG�BG�BG�BG�BH�BH�BL�BR�BP�BP�BP�BP�BP�BP�BQ�BT�BT�BT�BXBXBXBW
BXBXBYBYBYBW
BW
BW
BVBVBW
BYB^5B_;B`BB[#BN�BO�BN�BO�BR�BR�BYBZBYB[#BbNBffBhsBjBl�Bl�Bn�Bs�Bq�Bu�Bz�B�B�B�B�B�B~�B|�B�B�+B�B�B�B�1B�\B�hB�{B��B��B��B��B��B��B�B�LB�XB�jB�}BŢB��B��B��B��B��B��B��B�B�#B�TB�fB�mB�fB�TB�mB�B�B�B�B�B�B�B��B�B�B��B	%B	1B	1B	DB	�B	�B	�B	�B	�B	�B	!�B	$�B	#�B	#�B	)�B	-B	1'B	49B	49B	7LB	@�B	D�B	G�B	H�B	I�B	K�B	N�B	O�B	Q�B	R�B	S�B	W
B	XB	XB	XB	\)B	_;B	_;B	_;B	`BB	aHB	aHB	bNB	bNB	cTB	bNB	aHB	dZB	e`B	gmB	iyB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	jB	m�B	q�B	r�B	r�B	r�B	r�B	v�B	y�B	{�B	}�B	�B	�B	�B	�B	�1B	�=B	�DB	�PB	�VB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�^B	B	ĜB	ǮB	ɺB	ȴB	ƨB	ĜB	ŢB	ƨB	ŢB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�IB	��B
�B
"B
kB
%B
-�B
3�B
;JB
BB
G�B
J�B
O(B
S[B
Y�B
^�B
ezB
i�B
p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?m�B?�?}�?�?	r?�-�BD�>�0?)2a?�l�BD?/�{A��}?X?8B Ag��>�/�>��?�{?���>�{?B�xA'HB6\B.lA]J�?JzcB	�>�F�?�A�$?e�@� B9�A���?��?0��?�*B7�?1�@��BQ�@�?;�!A�=�?(�A��O>��*?s�BB1x>a>d�>��j>�5>�'x>�9?Q�o?��$?'@�A��A[0w?դ�@8bNA��?א?oYB5�A��?W6YA�,�B8?6��@�*�B9�?�On?K��A�Y?>�s�?'�A�:�B��>��@w�>���>���>���?\Af�A�9;?Y��B|�@�v8?�1?�cI?;,�A�:B�C?	��?�B5�B8e?{p?/��@�*�B6�@��>�;?*Av�X?/�@K0B7&B7�?���?l��B7|B6�BK�?�
5B;�>�5	?S�A��>Ѫ�?�5? �z?��B8�BF5?���B�1?34�A �B6�B`'?F�B:�? z?��B8=B>$?<��A���B8�@&M�?k�@S\�A��B��?(�'A��?1�A"��?�d|B7�B<�>��>��>�f�?�\?zÇB2�B<)>�X�?c)�?!'=?�T�B7�A�DI?'u�@{�B7�B;�>�3�?��A��B8hB:�@�+�@��?�ŽB8h@�XB6�B8�A'�?�j?NL�?�,�A��B7�B8B?�?��,A��p@}ՈB8 B7�B_?o�`?��/A��BB8�B��?]{�B9B8�B9#@#�Q?;l�A���B9�B8�B8vB-�AV�@��
@��iB8tB9 B9=B8NB9)A˙TA!O|A4��B8�B9�A��)B8�B:�B��B9QB8B87B8B9vB:,B9 B8�B9)B8�B8vB8�B9�B8�B7�B8B9�B9 AE��B9=B8�B8	B9 B7�B8�B8B8EB9qB8WB8�B7�B9�B8B8�B94B8bB8�B8�B9+B8�B9�B8�B8�B8B9qB8�B9B8_B9B8B8�B8�B9eB9eB9B9bB9�B9B9QB9	B7�B8|B9�B9 B9�B9�B8�B::B9�B9hB8�B8�B9qB9B9 B39B9zB9@B9@B9hB8NA���B:B8�B8�B97B9@B9B9ZB9QB8NB9QB9qB8�B8�B9�B:�A���B9�B7�B9B8WB7�B8�B9�B9�B9QB7SB9)B;wB9�B;�B8�B8�B;B:�B8�B8eB8�B:�B:FB9tB8�B8eB9�B8vBA%B9|B8 B8�B8�B8�B8�B97B8�B8�B8�B9�B8�BG�A�v�B8�B8QB8�B8�B9�B9�B9�B8�B97B9tB:NA��6B9�B8�B<�B9@B9�B8B8�B:wB8�B8nB8�B9�B9�B:FB8�B8�B8�B8QB8�B:B9@B97B9@B97B9�B9kB9�B9�B9|B8�B9tB9�B8�B9#B9B9B9|B:ZB9_B9�B9B8�B:`B9IB8�B9@B9,B9#B8eB97B8�B:wB9�B97B9eB::B9 B:B9�B97B9#B9�B8vB9�B9�B9�B:nB9�B9�B9�B8�B:nB9 B:BF�B;&B9@B9#B9tB9tB9B9@B9�A��B9,B8 B9�B9�B;IB:B9tB9kB9#B9tB9B9tB9|B8�B9tB9�B8�B:B:B9�B8�B97B9B8�B9�B8�B9(B9B9	B8�B9�B9�B8�B:B:B: B: B:B9�B9{B9*B9{B9�B8�B8�B8�B9�B8�B9
B9
B9B9�B9�B9IB9@B9B9B8�B8�B8�B9=B9\B9SB9KB9BB8gB90B9�B9�B8�B8�B8�B8�B8CB9RB9B8�B8�B9bB8�B9QB9?B:YB9~B9�B9�B:%B9rB8�B9�B8�B8�B9�B:cB:2B9WB9�B9�B9HB9|B9�B9bB9�B8�B9�B9UB8�B9�B8�B8�B9-B9kB8hB9ZB9IB9�B9�B9vB8�B8jB9<B9{B8�B9BB9aB:!B9�B8lB:XB9�B9�B8�B9ZB9B9�B:=B9B:+B8�B8�B9B: B9B8�B9
B8�B8�B9�B9iB9�B9OB8�B9�B9�B9B8�B8�B9YB9�B9�B8�B9�B8�B9�B8�B8�B9*B9^B8>B8�B85B8�B9WB:�B:`B9�B:NB:=B:B:�B:vB9�B:�B;B9PB8�B8�B8�B:DB:�B;�B:qB;FB:�B9�B9tB:�B:�B:sB:�B;B:B:+B9PB:aB9�B8�B9B9�B9�B9�B9@B9`B8�B9�B8�B:�B9�B9yB9�B:B:B9�B9�B9B;B9IB7CB9�B:�B;�B:�B;�B;tB8�B9�B<_B;�B<�B;�B:uB;pB;�B:B;�B;�B<�B<�B;�B<5B=IB;�B;�B<TB9YB?/B<NB<NB	��B	�`B	��B	��B	��B	�9B	��B	�jB	�B	��B	��B	��B	��B	�>B	�]B	�2B	��B	��B	��B	�KB	�B	��B	��B	��B	��B	�|B	��B	��B	�3B	�BB	��B	��B	�B	�MB	�TB	��B	��B	�FB	��B	��B	��B	��B	��B	�ZB	��B	��B	�B	��B	��B	�B	��B	�oB	�~B	�	B	��B	�XB	�zB	�`B	��B	�VB	��B	�6B	��B	��B	�+B	��B	�?B	�B	��B	�9B	��B	��B	�B	��B	�B	�WB	��B	��B	�B	��B	�jB	�]B	��B	��B	��B	�B	��B	�SB	�RB	�uB	��B	��B	�`B	��B	�B	�=B	��B	�B	��B	�%B	��B	�PB	��B	�kB	�B	�3B	�?B	�(B	��B	��B	��B	�pB	��B	�B	��B	�mB	��B	�eB	��B	��BoBoBo^BoUBouBowBo�Bn�BoqBo�Bn�Bn�Bo(BoBnOBoBoBn�Bo�Bo[Bo>BoIBoABo�Bn�Bn�BoeBn�BoBoBo�Bn�Bo�Bn�BoaBnqBoOBo)Bo]BneBn�Bn�BoyBn�Bo�BoVBn�Bn�BoBoBoABoCBn�Bn�Bn�Bn�Bn�Bn�BoBnsBoBo4Bo"BoBo�Bn�Bo�Bn�Bn�Bo�Bn�Bn�BonBn�Bo�Bn�Bn�Bn�Bn�Bn�BoBn�Bo9Bn�BolBn�Bn�Bn2Bn�Bo8Bo�Bn�Bo�Bn�Bn�Bn�BoBn�Bn�Bn�Bo{Bn�Bo9Bo0Bo�Bo�BoRBoIBoABn�Bp[BpdBn�Bo�BpBp$Bo�BoCBo	Bo1Bn�Bo�Bn�BovBo�Bo�BpBo�Bo�Bo"BpBpBn�Bn�Bo`Bo�Bo�Bo�BoQBo�Bo7Bo9Bo�Bo(BoBo�Bo�Bo�Bn�BoFBpBo3Bo�Bo*BoJBo�BpBoXBo�Bp~Bo�BpBpdBoWBpBpCBp/Bo�Bo�Bo:BoBpBp�Bp�Bp$BpkBpEBo�BqBp@Bo�Bp�Bp�Bp�Bo�BpjBp�Bo�Bo�Bo�Bp�Bp{Bq�Bp�Bp�B	ЅB	� B	шB	��B	эB	��B	ђB	�HB	��B	�MB	��B	�CB	�&B	��B	��B	�?B	��B	�B	ГB	��B	��B	��B	��B	ρB	ЍB	ϸB	�RB	ϰB	�B	��B	ΜB	��B	ΧB	ͯB	�&B	�2B	�'B	ϖB	ұB	�B	�7B	�KB	�PB	�tB	��B	�cB	�5B	�B	��B	҆B	іB	�JB	�B	�9B	�B	ѦB	ѫB	��B	� B	љB	�BB	�(B	ѤB	ЩB	�0B	�%B	�<B	�WB	�eB	�
B	�B	�_B	��B	��B	�!B	�DB	�2B	�B	��B	�(B	�NB	�'B	�hB	�?B	�B	ӌB	�pB	�pB	�mB	�1B	�TB	�9B	�B	�!B	ԯB	�B	�>B	��B	ղB	�XB	��B	��B	ԢB	�)B	ԾB	ԋB	ԈB	ӔB	�_B	�rB	�LB	ԪB	��B	ӔB	ӳB	ӕB	�9B	��B	ӛB	�DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943444434443444444444444334434444433444344344443443444444444344434433433443443443344444444434444434433444344444433443334344444443343443343443343344443444443344444334444334433444334443433444443334443334443343334443333444333333443343333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   Bo�Bo�Bn�Bn}Bn}Bn{Bn}Bo�Bo�Bo�Bo�Bo�Bp�Bp�Bo�Bo�Bp�Bq�Bq�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bw�Bx�Bx�Bx�Bx�Bx�By�By�By�B{�B}�B~�B�B�B��B��B�B�B}�B{�By�Bu�Bq�BmwBgTBW�BF�B86B3B6'B86B3B5%B86BK�B]B^B^B]BZ BW�BS�BP�BN�BG�B0B3B9<B:@B87B*�B{B	B�B�:B��B�jB��B��B�CBp�B[BT�BH�B98B7/B.�B#�B�B��B�dB�B��B�RBS�B
��B
�B
�{B
�DB
ɛB
��B
�,B
o�B
P�B
2B
%�B
�B
 B	ʤB	�,B	��B	��B	�zB	y�B	L�B	@cB	(�B	B		B��B�\B��BκB̬BƉB�iB�HB�&B�B��B��B��B�_B�jB�xB�pB�:B�B�B�B��B��B��B~�B{�Bx�Bv�Bv�Bv�Bv�Bu�Bs�Bu�Bw�Bw�Bw�B}�B��B��B��B�B�B��B��B�B~�B~�B|�By�Bu�Bq�Bk`BgIBe:Bc0Ba"B^BW�BU�BS�BR�BQ�BQ�BP�BN�BJ�BH�BF�BDyBBgBCpBCtBBiBDvBF�BE~BAeB>SBF�BO�BQ�BR�BS�BQ�BO�BM�BK�BH�BG�BG�BG�BG�BH�BH�BL�BR�BP�BP�BP�BP�BP�BP�BQ�BT�BT�BT�BW�BW�BW�BV�BW�BW�BX�BX�BX�BV�BV�BV�BU�BU�BV�BX�B^B_B`BZ�BN�BO�BN�BO�BR�BR�BX�BY�BX�BZ�Bb)Bf@BhNBjWBlcBlcBnqBs�Bq�Bu�Bz�B��B��B��B��B��B~�B|�B��B�B��B��B��B�B�4B�AB�SB�`B�gB�fB�xB��B��B��B�&B�1B�BB�VB�zBίBмB��B��B��B��B��B��B��B�-B�=B�EB�>B�-B�FB�uB�hB�^B�YB�XB�hB�B��B�qB�\B��B	�B		B		B	B	YB	_B	fB	sB	�B	�B	!�B	$�B	#�B	#�B	)�B	,�B	0�B	4B	4B	7&B	@[B	DsB	G�B	H�B	I�B	K�B	N�B	O�B	Q�B	R�B	S�B	V�B	W�B	W�B	W�B	\ B	_B	_B	_B	`B	aB	aB	b'B	b&B	c.B	b'B	aB	d1B	e:B	gFB	iSB	hLB	hJB	hJB	hOB	hIB	hJB	hJB	hMB	jVB	mjB	q�B	r�B	r�B	r�B	r�B	v�B	y�B	{�B	}�B	��B	��B	��B	��B	�B	�B	�B	�'B	�,B	�4B	�?B	�NB	�TB	�SB	�XB	�^B	�eB	�hB	�xB	�xB	�~B	�B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�
B	�B	�B	�#B	�6B	�fB	�uB	ǄB	ɒB	ȌB	ƁB	�tB	�zB	�~B	�yB	�xB	ƁB	ǄB	ǄB	ɑB	˝B	ͬB	αB	βB	ϷB	ϸB	ϷB	мB	мB	мB	ϷB	βB	��B	��B	нB	��B	��B	��B	��G�O�B	��B	�B	��B
�B
�B
DB
$�B
-�B
3�B
; B
A�B
G�B
J�B
OB
S4B
Y�B
^�B
eSB
i�B
p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B?�G�O�G�O�G�O�G�O�BD�G�O�G�O�G�O�BD G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6DB.QG�O�G�O�B	�G�O�G�O�G�O�G�O�G�O�B9�A��G�O�G�O�G�O�B7�G�O�G�O�BQnG�O�G�O�G�O�G�O�A��%G�O�G�O�B1_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�A��[G�O�G�O�B5�A��G�O�A�,|B7�G�O�G�O�B9zG�O�G�O�A�YG�O�G�O�A�:�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B|sG�O�G�O�G�O�G�O�G�O�B�(G�O�G�O�B5�B8KG�O�G�O�G�O�B6�G�O�G�O�G�O�G�O�G�O�G�O�B7B7�G�O�G�O�B7eB6�BK�G�O�B;�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B8~BFG�O�B�G�O�G�O�B6�B`
G�O�B:�G�O�G�O�B8 B>
G�O�A���B8oG�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�B7�B<wG�O�G�O�G�O�G�O�G�O�B2�B<G�O�G�O�G�O�G�O�B7�A�DG�O�G�O�B7{B;�G�O�G�O�G�O�B8MB:�G�O�G�O�G�O�B8MG�O�B6�B8�G�O�G�O�G�O�G�O�G�O�B7�B7�B?�G�O�G�O�G�O�B8B7�B^�G�O�G�O�G�O�B8mB��G�O�B8�B8�B9	G�O�G�O�G�O�B9gB8�B8YB-�G�O�G�O�G�O�B8YB9B9 B8/B9A˙)G�O�G�O�B8�B9�G�O�B8�B:�B��B93B8cB8B7�B9ZB:B9B8�B9B8�B8YB8�B9gB8�B7�B7�B9�B9G�O�B9 B8�B7�B9B7�B8�B7�B8*B9UB8<B8�B7�B9�B8cB8�B9B8GB8�B8oB9B8mB9�B8�B8�B7�B9UB8�B8�B8AB8�B8cB8�B8�B9IB9IB8�B9CB9pB8�B93B8�B7lB8aB9�B9B9�B9�B8|B:!B9�B9KB8�B8vB9SB8�B9B3!B9\B9&B9&B9IB8/A���B: B8�B8�B9B9&B8�B9?B93B8/B93B9UB8�B8�B9�B:�A���B9�B7�B8�B8<B7{B8�B9�B9�B93B78B9B;]B9�B;�B8�B8�B:�B:mB8�B8GB8�B:�B:*B9[B8�B8GB9�B8[BA
B9]B8B8�B8�B8�B8�B9B8�B8�B8tB9�B8�BG�A�v�B8�B87B8�B8�B9�B9uB9jB8�B9B9[B:1A��
B9�B8�B<�B9&B9jB7�B8�B:_B8�B8QB8�B9�B9�B:*B8�B8~B8�B87B8�B9�B9&B9B9&B9B9�B9KB9�B9�B9]B8�B9[B9�B8�B9	B8�B8�B9]B:?B9AB9�B8�B8�B:HB9+B8�B9&B9B9	B8GB9B8�B:_B9�B9B9IB:B9B9�B9uB9B9	B9�B8[B9�B9�B9�B:TB9�B9�B9�B8�B:TB9B9�BF�B;B9&B9	B9[B9[B8�B9&B9�A��B9B7�B9�B9�B;.B9�B9[B9KB9	B9[B8�B9[B9]B8~B9[B9�B8�B: B:eB9�B8�B9B9cB8�B9�B8jB9B9B8�B8�B9�B9�B8�B9�B9�B9�B9�B9�B9cB9_B9B9_B9�B8�B8�B8�B9uB8�B8�B8�B8�B9�B9�B9+B9"B8�B8�B8�B8�B8�B9"B9BB98BoBn�BoCBo<BoXBoXBo�Bn�BoTBoqBn�Bn�BoBoBn3Bn�Bn�Bn�BooBo>Bo#Bo.Bo#BozBn�Bn�BoKBn�Bn�Bn�BozBn�BoBn�BoIBnTBo3BoBo@BnJBn}BnvBo_Bn�Bo�Bo<Bn�Bn�Bn�Bn�Bo#Bo%Bn�Bn�Bn�Bn�Bn�Bn�Bn�BnXBn�BoBo
Bn�BomBn�Bo�Bn�Bn�Bo�Bn�Bn�BoRBn�BojBn�Bn�Bn�Bn�Bn�Bn�Bn�Bo!Bn�BoRBn�Bn�BnBn�BoBo�Bn�Bo�Bn�Bn�Bn�BodBn�Bn�Bn�Bo_Bn�BoBoBozBofBo7Bo*Bo%BndBp?BpGBn�Bo�Bo�BpBo�Bo(Bn�BoBn�BoqBn�BoXBo�BomBo�Bo�Bo�BoBpBo�Bn�Bn�BoCBo�BomBo�Bo7BomBoBoBo�BoBn�BovBo�Bo�Bn�Bo*Bo�BoBo�BoBo.BomBo�Bo>Bo�BpbBo�BpBpIBo;Bo�Bp'BpBo�Bo�BoBn�Bo�Bp�Bp�BpBpQBp(Bo�BqBp#BovBppBp�Bp�Bo�BpNBpdBo�Bo�Bo�Bp�Bp^Bq�Bp�Bp�B	�YB	��B	�aB	ЬB	�dB	��B	�iB	� B	��B	�&B	ЮB	�B	� B	��B	ЯB	�B	бB	��B	�jB	��B	ϻB	��B	ϰB	�[B	�eB	ϑB	�*B	τB	��B	ϚB	�rB	βB	�|B	͆B	��B	�B	��B	�nB	҉B	��B	�B	�!B	�'B	�LB	ҴB	�;B	�B	��B	��B	�\B	�mB	�#B	��B	�B	��B	�}B	уB	ѥB	��B	�pB	�B	� B	�{B	�B	�B	��B	�B	�0B	�;B	��B	��B	�6B	ќB	үB	��B	�B	�	B	��B	ҡB	� B	�$B	�B	�AB	�B	��B	�cB	�GB	�HB	�FB	�	B	�+B	�B	��B	��B	ԅB	��B	�B	��B	ՈB	�/B	��B	��B	�zB	� B	ԕB	�bB	�`B	�kB	�7B	�IB	�"B	ԀB	ӦB	�kB	ӉB	�lB	�B	ӧB	�tB	�BoBn�BoCBo<BoXBoXBo�Bn�BoTBoqBn�Bn�BoBoBn3Bn�Bn�Bn�BooBo>Bo#Bo.Bo#BozBn�Bn�BoKBn�Bn�Bn�BozBn�BoBn�BoIBnTBo3BoBo@BnJBn}BnvBo_Bn�Bo�Bo<Bn�Bn�Bn�Bn�Bo#Bo%Bn�Bn�Bn�Bn�Bn�Bn�Bn�BnXBn�BoBo
Bn�BomBn�Bo�Bn�Bn�Bo�Bn�Bn�BoRBn�BojBn�Bn�Bn�Bn�Bn�Bn�Bn�Bo!Bn�BoRBn�Bn�BnBn�BoBo�Bn�Bo�Bn�Bn�Bn�BodBn�Bn�Bn�Bo_Bn�BoBoBozBofBo7Bo*Bo%BndBp?BpGBn�Bo�Bo�BpBo�Bo(Bn�BoBn�BoqBn�BoXBo�BomBo�Bo�Bo�BoBpBo�Bn�Bn�BoCBo�BomBo�Bo7BomBoBoBo�BoBn�BovBo�Bo�Bn�Bo*Bo�BoBo�BoBo.BomBo�Bo>Bo�BpbBo�BpBpIBo;Bo�Bp'BpBo�Bo�BoBn�Bo�Bp�Bp�BpBpQBp(Bo�BqBp#BovBppBp�Bp�Bo�BpNBpdBo�Bo�Bo�Bp�Bp^Bq�Bp�Bp�B	�YB	��B	�aB	ЬB	�dB	��B	�iB	� B	��B	�&B	ЮB	�B	� B	��B	ЯB	�B	бB	��B	�jB	��B	ϻB	��B	ϰB	�[B	�eB	ϑB	�*B	τB	��B	ϚB	�rB	βB	�|B	͆B	��B	�B	��B	�nB	҉B	��B	�B	�!B	�'B	�LB	ҴB	�;B	�B	��B	��B	�\B	�mB	�#B	��B	�B	��B	�}B	уB	ѥB	��B	�pB	�B	� B	�{B	�B	�B	��B	�B	�0B	�;B	��B	��B	�6B	ќB	үB	��B	�B	�	B	��B	ҡB	� B	�$B	�B	�AB	�B	��B	�cB	�GB	�HB	�FB	�	B	�+B	�B	��B	��B	ԅB	��B	�B	��B	ՈB	�/B	��B	��B	�zB	� B	ԕB	�bB	�`B	�kB	�7B	�IB	�"B	ԀB	ӦB	�kB	ӉB	�lB	�B	ӧB	�tB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943444434443444444444444334434444433444344344443443444444444344434433433443443443344444444434444434433444344444433443334344444443343443343443343344443444443344444334444334433444334443433444443334443334443343334443333444333333443343333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649492020083116494920200831164949202008311649492020083116494920200831164949202008311649492020083116494920200831164949202008311649492020083116494920200831164949AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817242019021918172420190219181724    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817242019021918172420190219181724  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817242019021918172420190219181724  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649492020083116494920200831164949  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                