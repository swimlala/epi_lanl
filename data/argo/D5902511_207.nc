CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  w   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-04-06T09:09:59Z creation; 2023-02-10T23:09:44Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  _�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � !�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � =x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Dh   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` `    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   `�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   f�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   l�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T r�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   r�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   r�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   r�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   r�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � r�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   st   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   s�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    s�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        s�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        s�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       s�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    s�Argo profile    3.1 1.2 19500101000000  20220406090959  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_207                 6810_008521_207                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��Sd���@��Sd���11  @��S�O�;@��S�O�;@0b�FI�m@0b�FI�m�dҷ`,��dҷ`,�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@E�@}p�@�p�@�p�@޸RA ��AG�A!G�A,(�A@��AaG�A�Q�A��A��A�\)A��A�  A�Q�A�Q�B (�B  B�B�B�
B'�
B0  B8(�B@(�BG�
BP  BX  B`  Bh  Bo�Bw�
B�
B�{B�(�B�{B�  B��
B�  B�  B�{B�  B�  B��B��
B��B�  B�  B�{B�{B�  B�(�B�{B��B��B�  B�  B�{B��B�  B�  B�  B�  B�  C   C��C  C
=C
=C	��C�C��C��C�C�C  C
=C
=C  C  C��C"
=C$  C&  C(  C*
=C,  C-�C/��C2  C4  C5��C7��C:
=C<
=C=��C@  CA��CD  CF  CH  CJ{CL
=CN  CO��CQ��CS��CU��CX  CZ  C\  C^  C`  Ca��Cc��Cf  Ch
=Cj  Cl
=Cn
=Cp  Cr  Ct  Cv  Cx  Cz
=C|  C~
=C�  C�  C�  C�  C�C�  C���C�  C���C�  C�
=C�  C�C���C�  C�  C�
=C�C�C�  C���C�  C�  C�  C�  C���C���C���C�C�  C���C�  C�  C�C���C�  C�  C���C���C�C�C�  C�C�C�C�
=C�  C�
=C�  C�  C�  C�  C�  C���C���C�C�C���C���C�C�  C�C�C�  C�  C�
=C�  C���C�C���C�  C���C���C���C���C�C�  C�C�  C�  C�  C���C���C�
=C�C�
=C�
=C�C�  C���C�C�C�  C�  C�
=C�C���C���C�C�C���C���C���C�  C���C�  C�
=C�  C�
=C�
=C�  C���C�
=C�C���C�C���C���C���C�C�  C���C���C�  C�  C�  C���C���C���D � D  D}qD��D}qD�qD� D�D}qD��D}qD�qD� D�D�D�D�D	D	}qD	��D
}qD
�qD� D�D� D  D� D��D}qD�D� D  D��D�qDz�D  D��DD��D�qD� D  D��DD��D  D��DD�D�D��D�D� D  D� D  D�D�D��D�qDxRD��D}qD��D z�D!  D!�D"  D"}qD#  D#� D$  D$��D%�D%��D&�D&� D'  D'� D'��D(� D)�D)��D)�qD*� D+D+��D,�D,z�D,�RD-}qD.�D.� D.��D/}qD/�qD0�D1D1}qD1�qD2� D3  D3� D3�qD4}qD4��D5z�D5�qD6}qD6��D7z�D7��D8}qD8�qD9� D9��D:z�D:�qD;}qD<  D<�D=�D=}qD>  D>�D?D?}qD?��D@� D@�qDA}qDB  DB��DCDC��DC�qDDz�DD�qDE� DF  DF}qDG  DG��DH�DH��DI�DI��DJ�DJ��DKDK��DL  DL��DL�qDM}qDM�qDN��DO�DO}qDO��DPz�DP�RDQxRDR  DR�DS  DS}qDT�DT��DU  DU}qDV  DV}qDV�qDW� DW�qDX}qDYDY� DZ  DZ� D[�D[�D\D\�D]  D]��D^  D^� D_�D_��D`�D`}qD`��Da}qDb�Db�DcDc}qDd  Dd� Dd��De}qDf�Df� Dg  Dg� Dh  Dh��Di�Di}qDj  Dj}qDk  Dk}qDk��Dl� Dm  Dmz�Dm�qDn� Do  Do�Dp  Dp��Dq  Dq� Dq�qDrz�Dr�qDs}qDs�qDt� Du  Du� Dv  Dv� DwDw�Dx�Dx� Dy  Dy� Dz�Dz� D{  D{� D{�qD|� D}D}� D~�D~� D~�qD}qD�qD�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�B�D��HD���D��qD�@ D��HD�� D�  D�AHD���D�D��D�AHD��HD��HD�HD�AHD��HD�D��D�+�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?k�?u?�=q?u?�33?Ǯ?��@\)@��@&ff@@  @Tz�@c�
@u@�ff@��@���@��
@���@�Q�@�G�@�{@�Q�@�  @���@�Q�A ��A�A
�HA��A�A=qA ��A%�A(��A0  A5�A8Q�A=p�AC�
AG�AL��AQ�AUA\(�A`��Ac�
Ah��Amp�AqG�Aw
=A{�A~{A���A�z�A��RA�Q�A��A�{A�  A�=qA�p�A�  A�=qA�z�A�\)A���A�33A�{A���A��\A�A�Q�A��\A���A�  A��\A���A�\)A\A��A�
=A�=qA��A�
=A��A��A�
=A�=qA�p�A�\)A�=qA�p�A�\)A�=qA��A�
=A��A�p�A��A��A��A�\)B ��B�RB�B��B�\B�
B��B
ffB�
B��B�\B  B��B�\B  B�B�\B(�BG�B�RBQ�BG�B�RB Q�B!p�B"�HB$Q�B%p�B&�\B((�B)��B*�RB,Q�B-B.�HB0Q�B1�B2�HB4(�B5B6�HB8  B9p�B:�HB;�B<��B>�RB@  BA�BB�\BC�BD��BF�\BG�BH��BI�BK�BL��BN{BO�BQG�BR�\BS�BU�BV�RBW�
BYG�BZ�HB\(�B]�B^�\B`(�Bap�BbffBd  Be��Bf�RBg�
BiG�Bj�HBl  Bm�Bn�HBp(�Bq�Br�RBtQ�Bup�Bv�HBxz�By��Bz�\B|(�B}��B~�\B�
B��RB��B��B�ffB��HB�\)B�  B��\B��HB�\)B�  B�ffB���B�33B��B��B�Q�B���B�
=B�\)B��B�(�B�ffB���B��B�p�B��B�Q�B��\B���B�\)B���B��
B�ffB��RB���B�\)B��
B�{B�ffB���B�33B�p�B��B�Q�B�z�B���B�\)B��B��B�=qB���B�
=B�G�B��B�(�B�=qB��RB��B�G�B���B�{B�z�B���B���B��B�B��B�ffB���B���B�G�B�B�{B�=qB���B�
=B�G�B���B�  B�z�B��RB���B�p�B��B��
B�=qB��RB��HB��B��B��
B�{B�z�B���B���B�\)B�B��B�{B���B��HB�
=B�p�B��
B�{B�=qB��\B���B�33B�\)B��
B�{B�z�B���B��HB�G�B��B��B�{B�ffB��HB�33B�\)B���B�(�B�z�B��RB���B�p�B��
B�{B�Q�B���B�33B�p�B�B�=qB���B���B�33B���B�  B�Q�B��\B�
=B�p�B���B�  B�z�B��RB��HB�G�B��
B�  B�=qB���B��B�p�B��B��B�Q�B���B��HB�33B���B�  B�=qB�ffB��RB�33B��B�B�  B�Q�B��RB��B�G�B��B�  B�ffB���B���B��B��B��B�(�B�ffB���B��B��B��B��B�Q�B¸RB�
=B�33B�p�B��B�=qB�Q�Bď\B�
=B�\)Bř�B�B�{B�z�B���B���B��BǅB��B�(�B�ffBȣ�B��B�p�BɮB��B�ffBʸRB���B�33B˙�B�{B�Q�B�z�B��HB�\)BͮB��B�(�BΣ�B�
=B�\)Bϙ�B��
B�=qBУ�B�
=B�33BхB��B�Q�Bҏ\B���B�G�BӅBӮB�{Bԣ�B���B���B�p�B��
B�(�B�ffB֣�B��BׅB�B�  B�z�B���B���B�G�BٮB�(�B�ffBڏ\B�
=BۅB�B��B�ffB���B���B�G�B�B�{B�=qBޣ�B�33Bߙ�B��
B�(�B��B�33B�B�B�Q�B���B��B�\)B��
B�ffB�\B�
=B噚B�  B�=qB�RB�33B癚B��
B�ffB���B�
=B�B�  B�Q�B��B�G�B뙚B��B�Q�B��HB�\)B홚B��B�ffB���B�G�BB�{B��B��HB�G�B��B�Q�B��B�
=B�B�{B�ffB���B�\)B��
B�{B�z�B��B���B��
B�ffB���B�G�B���B�=qB���B��HB�p�B�  B�Q�B���B�G�B�B�{B�ffB���B�p�B��
C �C Q�C ��C �HC  C33Cz�CC��C{C\)C��C�HC
=C33Cp�C�RC��C(�CQ�C��C�
C
=C33C�CC��C�CQ�C��C�
C  C(�CffC�C�C{C=qC�C��C�C	�C	\)C	��C	�HC
  C
33C
ffC
�C
�C�CG�Cp�C�C��C(�CQ�C�C��C
=C33C\)C�C�HC
=C=qC�CC��C(�CQ�C�\C�
C{CG�CffC��C�HC{C=qCffC�C�C�C=qCp�C�RC�C�C=qCffC��C�HC{CG�CffC�\CC  C=qCffC�\C�RC  C=qCp�C�\CC��C=qCp�C�\CC��C33Cp�C��CC�C33CffC��C��C�C�Cp�C��C��C�C{CQ�C�CC��C(�CQ�C�C��C�
C�C\)C�\C�RC�C{CG�C�\C��C  C33C\)C�CC   C =qC p�C �\C C ��C!33C!p�C!��C!C!�C"�C"ffC"��C"�
C#  C#(�C#\)C#��C#�HC${C$=qC$\)C$�\C$��C%
=C%G�C%p�C%��C%C%��C&33C&p�C&�C&�
C'  C'33C'ffC'��C'�HC({C(Q�C(z�C(��C(�
C){C)Q�C)�\C)��C*  C*(�C*\)C*�C*C*��C+33C+p�C+�C+�HC,{C,=qC,ffC,�C,�C-�C-\)C-�\C-�RC-�C.(�C.p�C.�C.�C/(�C/G�C/�C/��C0  C0G�C0�C0�RC0��C1�C1\)C1��C1�HC2�C2ffC2��C2�
C3
=C3G�C3z�C3C4
=C4=qC4z�C4C5  C533C5\)C5�\C5��C6  C6=qC6�C6��C7
=C7Q�C7�C7�RC7�C8(�C8ffC8�C8��C9G�C9�C9�RC9��C:33C:p�C:�C:�C;33C;�C;�RC<  C<G�C<�\C<�
C={C=Q�C=�\C=��C>
=C>Q�C>��C>�HC?(�C?p�C?�RC@  C@=qC@�C@CA  CA=qCAz�CACB
=CBG�CB��CB�
CC{CC\)CC��CC��CD33CD�CD��CE
=CEQ�CE�\CE��CF
=CFQ�CF�\CF�
CG{CGQ�CG�\CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�=q@�\@E�@}p�@�p�@�p�@޸RA ��AG�A!G�A,(�A@��AaG�A�Q�A��A��A�\)A��A�  A�Q�A�Q�B (�B  B�B�B�
B'�
B0  B8(�B@(�BG�
BP  BX  B`  Bh  Bo�Bw�
B�
B�{B�(�B�{B�  B��
B�  B�  B�{B�  B�  B��B��
B��B�  B�  B�{B�{B�  B�(�B�{B��B��B�  B�  B�{B��B�  B�  B�  B�  B�  C   C��C  C
=C
=C	��C�C��C��C�C�C  C
=C
=C  C  C��C"
=C$  C&  C(  C*
=C,  C-�C/��C2  C4  C5��C7��C:
=C<
=C=��C@  CA��CD  CF  CH  CJ{CL
=CN  CO��CQ��CS��CU��CX  CZ  C\  C^  C`  Ca��Cc��Cf  Ch
=Cj  Cl
=Cn
=Cp  Cr  Ct  Cv  Cx  Cz
=C|  C~
=C�  C�  C�  C�  C�C�  C���C�  C���C�  C�
=C�  C�C���C�  C�  C�
=C�C�C�  C���C�  C�  C�  C�  C���C���C���C�C�  C���C�  C�  C�C���C�  C�  C���C���C�C�C�  C�C�C�C�
=C�  C�
=C�  C�  C�  C�  C�  C���C���C�C�C���C���C�C�  C�C�C�  C�  C�
=C�  C���C�C���C�  C���C���C���C���C�C�  C�C�  C�  C�  C���C���C�
=C�C�
=C�
=C�C�  C���C�C�C�  C�  C�
=C�C���C���C�C�C���C���C���C�  C���C�  C�
=C�  C�
=C�
=C�  C���C�
=C�C���C�C���C���C���C�C�  C���C���C�  C�  C�  C���C���C���D � D  D}qD��D}qD�qD� D�D}qD��D}qD�qD� D�D�D�D�D	D	}qD	��D
}qD
�qD� D�D� D  D� D��D}qD�D� D  D��D�qDz�D  D��DD��D�qD� D  D��DD��D  D��DD�D�D��D�D� D  D� D  D�D�D��D�qDxRD��D}qD��D z�D!  D!�D"  D"}qD#  D#� D$  D$��D%�D%��D&�D&� D'  D'� D'��D(� D)�D)��D)�qD*� D+D+��D,�D,z�D,�RD-}qD.�D.� D.��D/}qD/�qD0�D1D1}qD1�qD2� D3  D3� D3�qD4}qD4��D5z�D5�qD6}qD6��D7z�D7��D8}qD8�qD9� D9��D:z�D:�qD;}qD<  D<�D=�D=}qD>  D>�D?D?}qD?��D@� D@�qDA}qDB  DB��DCDC��DC�qDDz�DD�qDE� DF  DF}qDG  DG��DH�DH��DI�DI��DJ�DJ��DKDK��DL  DL��DL�qDM}qDM�qDN��DO�DO}qDO��DPz�DP�RDQxRDR  DR�DS  DS}qDT�DT��DU  DU}qDV  DV}qDV�qDW� DW�qDX}qDYDY� DZ  DZ� D[�D[�D\D\�D]  D]��D^  D^� D_�D_��D`�D`}qD`��Da}qDb�Db�DcDc}qDd  Dd� Dd��De}qDf�Df� Dg  Dg� Dh  Dh��Di�Di}qDj  Dj}qDk  Dk}qDk��Dl� Dm  Dmz�Dm�qDn� Do  Do�Dp  Dp��Dq  Dq� Dq�qDrz�Dr�qDs}qDs�qDt� Du  Du� Dv  Dv� DwDw�Dx�Dx� Dy  Dy� Dz�Dz� D{  D{� D{�qD|� D}D}� D~�D~� D~�qD}qD�qD�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�B�D��HD���D��qD�@ D��HD�� D�  D�AHD���D�D��D�AHD��HD��HD�HD�AHD��HD�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?k�?u?�=q?u?�33?Ǯ?��@\)@��@&ff@@  @Tz�@c�
@u@�ff@��@���@��
@���@�Q�@�G�@�{@�Q�@�  @���@�Q�A ��A�A
�HA��A�A=qA ��A%�A(��A0  A5�A8Q�A=p�AC�
AG�AL��AQ�AUA\(�A`��Ac�
Ah��Amp�AqG�Aw
=A{�A~{A���A�z�A��RA�Q�A��A�{A�  A�=qA�p�A�  A�=qA�z�A�\)A���A�33A�{A���A��\A�A�Q�A��\A���A�  A��\A���A�\)A\A��A�
=A�=qA��A�
=A��A��A�
=A�=qA�p�A�\)A�=qA�p�A�\)A�=qA��A�
=A��A�p�A��A��A��A�\)B ��B�RB�B��B�\B�
B��B
ffB�
B��B�\B  B��B�\B  B�B�\B(�BG�B�RBQ�BG�B�RB Q�B!p�B"�HB$Q�B%p�B&�\B((�B)��B*�RB,Q�B-B.�HB0Q�B1�B2�HB4(�B5B6�HB8  B9p�B:�HB;�B<��B>�RB@  BA�BB�\BC�BD��BF�\BG�BH��BI�BK�BL��BN{BO�BQG�BR�\BS�BU�BV�RBW�
BYG�BZ�HB\(�B]�B^�\B`(�Bap�BbffBd  Be��Bf�RBg�
BiG�Bj�HBl  Bm�Bn�HBp(�Bq�Br�RBtQ�Bup�Bv�HBxz�By��Bz�\B|(�B}��B~�\B�
B��RB��B��B�ffB��HB�\)B�  B��\B��HB�\)B�  B�ffB���B�33B��B��B�Q�B���B�
=B�\)B��B�(�B�ffB���B��B�p�B��B�Q�B��\B���B�\)B���B��
B�ffB��RB���B�\)B��
B�{B�ffB���B�33B�p�B��B�Q�B�z�B���B�\)B��B��B�=qB���B�
=B�G�B��B�(�B�=qB��RB��B�G�B���B�{B�z�B���B���B��B�B��B�ffB���B���B�G�B�B�{B�=qB���B�
=B�G�B���B�  B�z�B��RB���B�p�B��B��
B�=qB��RB��HB��B��B��
B�{B�z�B���B���B�\)B�B��B�{B���B��HB�
=B�p�B��
B�{B�=qB��\B���B�33B�\)B��
B�{B�z�B���B��HB�G�B��B��B�{B�ffB��HB�33B�\)B���B�(�B�z�B��RB���B�p�B��
B�{B�Q�B���B�33B�p�B�B�=qB���B���B�33B���B�  B�Q�B��\B�
=B�p�B���B�  B�z�B��RB��HB�G�B��
B�  B�=qB���B��B�p�B��B��B�Q�B���B��HB�33B���B�  B�=qB�ffB��RB�33B��B�B�  B�Q�B��RB��B�G�B��B�  B�ffB���B���B��B��B��B�(�B�ffB���B��B��B��B��B�Q�B¸RB�
=B�33B�p�B��B�=qB�Q�Bď\B�
=B�\)Bř�B�B�{B�z�B���B���B��BǅB��B�(�B�ffBȣ�B��B�p�BɮB��B�ffBʸRB���B�33B˙�B�{B�Q�B�z�B��HB�\)BͮB��B�(�BΣ�B�
=B�\)Bϙ�B��
B�=qBУ�B�
=B�33BхB��B�Q�Bҏ\B���B�G�BӅBӮB�{Bԣ�B���B���B�p�B��
B�(�B�ffB֣�B��BׅB�B�  B�z�B���B���B�G�BٮB�(�B�ffBڏ\B�
=BۅB�B��B�ffB���B���B�G�B�B�{B�=qBޣ�B�33Bߙ�B��
B�(�B��B�33B�B�B�Q�B���B��B�\)B��
B�ffB�\B�
=B噚B�  B�=qB�RB�33B癚B��
B�ffB���B�
=B�B�  B�Q�B��B�G�B뙚B��B�Q�B��HB�\)B홚B��B�ffB���B�G�BB�{B��B��HB�G�B��B�Q�B��B�
=B�B�{B�ffB���B�\)B��
B�{B�z�B��B���B��
B�ffB���B�G�B���B�=qB���B��HB�p�B�  B�Q�B���B�G�B�B�{B�ffB���B�p�B��
C �C Q�C ��C �HC  C33Cz�CC��C{C\)C��C�HC
=C33Cp�C�RC��C(�CQ�C��C�
C
=C33C�CC��C�CQ�C��C�
C  C(�CffC�C�C{C=qC�C��C�C	�C	\)C	��C	�HC
  C
33C
ffC
�C
�C�CG�Cp�C�C��C(�CQ�C�C��C
=C33C\)C�C�HC
=C=qC�CC��C(�CQ�C�\C�
C{CG�CffC��C�HC{C=qCffC�C�C�C=qCp�C�RC�C�C=qCffC��C�HC{CG�CffC�\CC  C=qCffC�\C�RC  C=qCp�C�\CC��C=qCp�C�\CC��C33Cp�C��CC�C33CffC��C��C�C�Cp�C��C��C�C{CQ�C�CC��C(�CQ�C�C��C�
C�C\)C�\C�RC�C{CG�C�\C��C  C33C\)C�CC   C =qC p�C �\C C ��C!33C!p�C!��C!C!�C"�C"ffC"��C"�
C#  C#(�C#\)C#��C#�HC${C$=qC$\)C$�\C$��C%
=C%G�C%p�C%��C%C%��C&33C&p�C&�C&�
C'  C'33C'ffC'��C'�HC({C(Q�C(z�C(��C(�
C){C)Q�C)�\C)��C*  C*(�C*\)C*�C*C*��C+33C+p�C+�C+�HC,{C,=qC,ffC,�C,�C-�C-\)C-�\C-�RC-�C.(�C.p�C.�C.�C/(�C/G�C/�C/��C0  C0G�C0�C0�RC0��C1�C1\)C1��C1�HC2�C2ffC2��C2�
C3
=C3G�C3z�C3C4
=C4=qC4z�C4C5  C533C5\)C5�\C5��C6  C6=qC6�C6��C7
=C7Q�C7�C7�RC7�C8(�C8ffC8�C8��C9G�C9�C9�RC9��C:33C:p�C:�C:�C;33C;�C;�RC<  C<G�C<�\C<�
C={C=Q�C=�\C=��C>
=C>Q�C>��C>�HC?(�C?p�C?�RC@  C@=qC@�C@CA  CA=qCAz�CACB
=CBG�CB��CB�
CC{CC\)CC��CC��CD33CD�CD��CE
=CEQ�CE�\CE��CF
=CFQ�CF�\CF�
CG{CGQ�CG�\CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��`A�JA�bA�
=A��A��A��A��A��A��A��A��A��A�A��#Aϥ�AυA�(�A�JA�A���A�1A�JA��A�"�A�(�A�1'A�9XA�?}A�M�A�bNA�O�A�bA���A��`A��;A��A���A�AξwAκ^AζFAβ-AΟ�AΑhA�v�A�E�Aͧ�A�ȴAɗ�A��A�O�A�A���A��A�A� �A���A�5?A���A�$�A�33A��A���A�|�A�hsA�t�A���A��A��hA�{A���A���A���A��7A�1'A��A�A��hA��TA���A�E�A�JA�S�A�|�A�ZA�A�A��TA�oA��DA��FA���A���A�r�A�+A���A��+A�VA��`A�VA�A��A{��Az-AyS�Av�At1'Aq�TAo�
AnM�Ak��Ah �Ae�FAb^5A^jA\�A[t�AY+AW�PAU`BARjAO��AM�AH��AF~�AD�!ACx�AA��A@�!A?|�A>jA<��A9%A7��A5��A4�A4{A3?}A1"�A.ffA+��A+A*jA)S�A(E�A'�A&�A%C�A#hsA"bNA!�-A!��A��A�A��A�`A�wA&�A�!AffAbNAVA9XA��A�A;dAjA�A5?AƨA�A�A~�A�`A
=AQ�A��A��A��AjA  A�RA�FAdZAx�Ap�AXAO�A��A�jA�At�A�wAr�AQ�Av�A�A;dAVAjAQ�A��A�A�AdZA?}AoA ��A ��A �HA �HA �A �HA ��A ~�A M�@��@��R@���@���@�J@�p�@�7L@�I�@���@���@�;d@�33@�M�@�Z@�P@��@�ƨ@�|�@��@���@�n�@��@�9X@�~�@��@��m@�S�@�33@��@�
=@���@��@�@��@�-@�7L@�z�@���@�K�@��@�hs@���@���@�(�@�dZ@߅@��@��#@�Ĝ@�9X@���@���@���@�"�@��@�hs@؛�@��;@�@֧�@�v�@�$�@Լj@�bN@�I�@�(�@�b@�  @���@ӶF@�;d@�ȴ@�@�/@�j@�b@��
@϶F@�l�@�n�@�@�hs@�V@�j@��@��@��;@˝�@�
=@ʟ�@�v�@�@ɑh@��`@ț�@�(�@��;@�l�@�
=@�5?@š�@�O�@ļj@�b@å�@�\)@�K�@��H@�V@�5?@�$�@�@���@�%@���@��H@���@�M�@��@�`B@�r�@��@�"�@�"�@�ȴ@�M�@�X@�G�@�?}@��@��/@��u@��;@�+@�E�@��@�J@�@�@��T@��7@�O�@��@���@�z�@� �@��F@�|�@�
=@�ȴ@�{@���@��7@�%@���@��9@�A�@��@�\)@�"�@���@�v�@�M�@���@�/@���@��j@�z�@�A�@� �@�\)@�~�@�=q@��T@��@�hs@�hs@�hs@�X@�%@��@��D@���@��P@�|�@�t�@�S�@�C�@�;d@��H@�M�@��@��T@�/@��j@��@��D@�bN@�9X@�1@��@��F@���@�K�@���@�V@�@�`B@��`@��9@��@�Z@�I�@�b@��w@�S�@�
=@��R@��+@�n�@�^5@�E�@�=q@�J@��@��#@��^@�X@��@��`@��`@�Ĝ@�z�@�bN@�9X@�ƨ@���@�t�@��@��H@�~�@�{@���@�x�@�X@�7L@�V@���@���@�bN@�  @���@��@�l�@�33@��@���@���@��7@�X@�7L@�&�@�%@��`@��u@�9X@��;@��P@�ȴ@���@��+@�ff@�n�@�J@��^@��7@��@��@� �@��m@���@�\)@��y@�E�@��#@��7@��@��@��`@��`@��/@�Ĝ@��9@���@��@�1'@���@�"�@�v�@���@��#@��-@��7@�7L@�1'@��;@��P@�"�@���@��H@���@�ȴ@���@�E�@�J@���@��@�p�@���@�j@�j@�z�@�9X@��@�t�@�\)@�C�@��@��@�o@��@��@��@��!@�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�Q�A�jAϙ�A�1A�A�
=A�oA�oA�1A��A�{A�%A�A�bA��A��A��A� �A��A��A� �A��A��A��A�"�A��A��A��A� �A��A��A� �A��A��A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�1A�A�  A���A��A��A���A��/A���A���AϺ^Aϰ!Aϴ9Aϧ�Aϩ�Aϡ�Aϝ�Aϝ�Aϛ�AϏ\AύPAυAρA�p�A�dZA�K�A�/A�{A��A�{A�VA�bA�VA�
=A�
=A�
=A�A�A�A�  A�A�A���A�  A�A���A���A���A�  A�A�JA�1A�1A�
=A�1A�%A�
=A�VA�JA�oA�{A�bA��A��A��A� �A� �A��A�"�A�&�A�"�A�&�A�&�A�"�A�&�A�+A�(�A�+A�1'A�-A�-A�5?A�5?A�1'A�;dA�9XA�7LA�9XA�=qA�7LA�9XA�=qA�9XA�9XA�A�A�E�A�C�A�E�A�K�A�M�A�M�A�S�A�VA�ZA�^5A�bNA�bNA�hsA�n�A�n�A�hsA�VA�E�A�7LA�&�A��A�{A�JA�
=A�
=A�%A�  A���A���A��A��A��yA��yA��TA��HA��TA��mA��TA��;A��TA��HA��/A��#A��/A��A��#A��#A��A���A���A���A���A���A�ȴA�A���A�ĜA�ĜA���A�A�ĜA���AξwA�ĜA���AμjA���A���AμjAξwA���AξwAμjA���AμjAμjAξwAκ^AθRAμjAκ^AζFAκ^AμjAθRAζFAκ^AθRAδ9AθRAκ^Aδ9Aδ9AθRAζFAβ-AζFAθRAδ9Aδ9AζFAζFAΰ!Aβ-Aδ9Aΰ!AήAβ-Aΰ!AάAήAΰ!AΥ�AΝ�AΛ�AΙ�AΓuAΓuAΗ�AΕ�AΓuAΓuAΕ�AΏ\AΏ\AΕ�AΑhAΏ\AΓuAΓuAΏ\AΏ\AΏ\AΑhA·+A΁A΃A΁A�z�A�p�A�n�A�ffA�bNA�ffA�dZA�^5A�`BA�ZA�O�A�O�A�O�A�E�A�C�A�E�A�=qA�5?A�/A�$�A��A�oA�
=A�  A��A��#A;wAͮA͛�A͋DA�r�A�I�A�9XA���A̕�A�x�A�O�A�"�A�
=A�A��A˶FA�bNA�bA��mA���A���Aʉ7A�-A��`AɬAɃA�bNA�(�A���A���Aș�A�x�A�jA�O�A�?}A�/A��A�bA�1A���A��A��A��A��mA��;A��
A���AǶFAǙ�AǃA�\)A���Aư!AƟ�Aƕ�AƅA�v�A�n�A�dZA�M�A�/A�{A��A��;A�ȴAŶFAũ�Aŕ�A�~�A�hsA�VA�C�A�1'A�$�A�{A�VA��A���Aġ�Aę�AāA�z�A�dZA�I�A�$�A��A�bA���A��A��TA��/A���AìA�|�A�S�A�9XA�+A�VA��mA���Aº^A¶FA¬A�A�x�A�;dA�  A���A��hA���A�hsA�5?A�&�A�$�A��A�VA���A��TA��A���A�A��wA���A��jA��FA���A���A���A���A��uA��A�|�A�z�A�z�A�hsA�Q�A�O�A�A�A�$�A��A��A�bA�VA�
=A�
=A�A���A��A��`A���A�A��jA��jA��RA��9A��-A��!A���A�v�A�  A���A�hsA�  A��!A���A��uA�z�A�bNA�ZA�XA�Q�A�O�A�O�A�7LA��;A��A�dZA���A���A�|�A�`BA�9XA���A���A���A�x�A�=qA�{A���A�t�A�$�A��A��A��`A���A��FA���A��A�ZA�C�A��A��A�ƨA�l�A�I�A�=qA�?}A�+A�bA���A��`A���A��-A�~�A�^5A�7LA��A���A��
A��^A���A���A��uA��A�dZA�A�A�7LA�/A�$�A�oA���A��mA���A���A���A��A�I�A�1'A��A�1A���A��;A��;A��/A��;A��;A���A��wA��-A��A��A���A��DA�VA�1'A��A�JA�1A��A��+A���A�r�A��A��uA�^5A�E�A�/A�"�A�1A��;A��^A��A���A�v�A�(�A��`A�ĜA���A��+A�jA�K�A�+A��A�1A���A��mA�ȴA���A��DA�Q�A�(�A�oA�
=A�A���A��`A��FA�t�A���A�A���A��\A�r�A�\)A�O�A�?}A�+A�bA��A��A��A�ffA�O�A�1'A�JA���A���A��PA�`BA�9XA�/A��A�  A���A���A���A���A��A���A���A��A��mA��^A���A��+A�p�A�I�A��A��HA���A�O�A�(�A��A�bA��;A���A�r�A�dZA�;dA�&�A�"�A��A�%A��!A��mA�XA�"�A��;A��^A��A���A���A��7A�t�A�ZA�=qA�JA��A��;A���A���A���A�v�A�+A�%A��A��^A���A�t�A�G�A�-A� �A�oA��`A��DA�/A��A���A�n�A�Q�A�I�A�I�A�?}A�
=A���A�?}A��
A���A�~�A�-A��HA���A�x�A�bNA�I�A�;dA��A�A��A��;A���A��wA��-A��uA�hsA�/A��yA��FA���A��DA�r�A�dZA�XA�C�A�"�A�A�^5A�;dA��A��A��A�XA�G�A�-A�bA���A��jA��A��PA�z�A�XA���A��jA���A�hsA�ZA�G�A�VA���A��PA�jA�A�A��A��
A��A�M�A�=qA��A�{A�JA���A��#A��FA���A��7A�ZA�C�A�1A�%A��HA�z�A�~�A�/A�l�A�ĜA�/A��A�A��/A���A�A��A�p�A�=qA��A���A�n�A�G�A�E�A�E�A�?}A�+A�%A���A�p�A�33A�A���A��A��;A��A���A�A��RA��9A���A���A�r�A�jA�jA�M�A�E�A�"�A��hA�7LA��TA��FA��A�S�A���A���A�XA��A��/A��DA�r�A�XA��A��`A��wA���A�ffA�"�A��A��A�ZA�1'A�(�A�"�A��A��A��A���A�XA��A�JA�A�hAoA~�A}&�A|VA{�A{�^A{��A{\)A{VAz�Az��Azn�AzI�Az-Az{Ay��Ay�TAy��Ay��Ay��Ayt�Ayl�AyXAy%Ax��Ax^5AxJAwp�Av��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A�JA�bA�
=A��A��A��A��A��A��A��A��A��A�A��#Aϥ�AυA�(�A�JA�A���A�1A�JA��A�"�A�(�A�1'A�9XA�?}A�M�A�bNA�O�A�bA���A��`A��;A��A���A�AξwAκ^AζFAβ-AΟ�AΑhA�v�A�E�Aͧ�A�ȴAɗ�A��A�O�A�A���A��A�A� �A���A�5?A���A�$�A�33A��A���A�|�A�hsA�t�A���A��A��hA�{A���A���A���A��7A�1'A��A�A��hA��TA���A�E�A�JA�S�A�|�A�ZA�A�A��TA�oA��DA��FA���A���A�r�A�+A���A��+A�VA��`A�VA�A��A{��Az-AyS�Av�At1'Aq�TAo�
AnM�Ak��Ah �Ae�FAb^5A^jA\�A[t�AY+AW�PAU`BARjAO��AM�AH��AF~�AD�!ACx�AA��A@�!A?|�A>jA<��A9%A7��A5��A4�A4{A3?}A1"�A.ffA+��A+A*jA)S�A(E�A'�A&�A%C�A#hsA"bNA!�-A!��A��A�A��A�`A�wA&�A�!AffAbNAVA9XA��A�A;dAjA�A5?AƨA�A�A~�A�`A
=AQ�A��A��A��AjA  A�RA�FAdZAx�Ap�AXAO�A��A�jA�At�A�wAr�AQ�Av�A�A;dAVAjAQ�A��A�A�AdZA?}AoA ��A ��A �HA �HA �A �HA ��A ~�A M�@��@��R@���@���@�J@�p�@�7L@�I�@���@���@�;d@�33@�M�@�Z@�P@��@�ƨ@�|�@��@���@�n�@��@�9X@�~�@��@��m@�S�@�33@��@�
=@���@��@�@��@�-@�7L@�z�@���@�K�@��@�hs@���@���@�(�@�dZ@߅@��@��#@�Ĝ@�9X@���@���@���@�"�@��@�hs@؛�@��;@�@֧�@�v�@�$�@Լj@�bN@�I�@�(�@�b@�  @���@ӶF@�;d@�ȴ@�@�/@�j@�b@��
@϶F@�l�@�n�@�@�hs@�V@�j@��@��@��;@˝�@�
=@ʟ�@�v�@�@ɑh@��`@ț�@�(�@��;@�l�@�
=@�5?@š�@�O�@ļj@�b@å�@�\)@�K�@��H@�V@�5?@�$�@�@���@�%@���@��H@���@�M�@��@�`B@�r�@��@�"�@�"�@�ȴ@�M�@�X@�G�@�?}@��@��/@��u@��;@�+@�E�@��@�J@�@�@��T@��7@�O�@��@���@�z�@� �@��F@�|�@�
=@�ȴ@�{@���@��7@�%@���@��9@�A�@��@�\)@�"�@���@�v�@�M�@���@�/@���@��j@�z�@�A�@� �@�\)@�~�@�=q@��T@��@�hs@�hs@�hs@�X@�%@��@��D@���@��P@�|�@�t�@�S�@�C�@�;d@��H@�M�@��@��T@�/@��j@��@��D@�bN@�9X@�1@��@��F@���@�K�@���@�V@�@�`B@��`@��9@��@�Z@�I�@�b@��w@�S�@�
=@��R@��+@�n�@�^5@�E�@�=q@�J@��@��#@��^@�X@��@��`@��`@�Ĝ@�z�@�bN@�9X@�ƨ@���@�t�@��@��H@�~�@�{@���@�x�@�X@�7L@�V@���@���@�bN@�  @���@��@�l�@�33@��@���@���@��7@�X@�7L@�&�@�%@��`@��u@�9X@��;@��P@�ȴ@���@��+@�ff@�n�@�J@��^@��7@��@��@� �@��m@���@�\)@��y@�E�@��#@��7@��@��@��`@��`@��/@�Ĝ@��9@���@��@�1'@���@�"�@�v�@���@��#@��-@��7@�7L@�1'@��;@��P@�"�@���@��H@���@�ȴ@���@�E�@�J@���@��@�p�@���@�j@�j@�z�@�9X@��@�t�@�\)@�C�@��@��@�o@��@��@��@��!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�Q�A�jAϙ�A�1A�A�
=A�oA�oA�1A��A�{A�%A�A�bA��A��A��A� �A��A��A� �A��A��A��A�"�A��A��A��A� �A��A��A� �A��A��A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�1A�A�  A���A��A��A���A��/A���A���AϺ^Aϰ!Aϴ9Aϧ�Aϩ�Aϡ�Aϝ�Aϝ�Aϛ�AϏ\AύPAυAρA�p�A�dZA�K�A�/A�{A��A�{A�VA�bA�VA�
=A�
=A�
=A�A�A�A�  A�A�A���A�  A�A���A���A���A�  A�A�JA�1A�1A�
=A�1A�%A�
=A�VA�JA�oA�{A�bA��A��A��A� �A� �A��A�"�A�&�A�"�A�&�A�&�A�"�A�&�A�+A�(�A�+A�1'A�-A�-A�5?A�5?A�1'A�;dA�9XA�7LA�9XA�=qA�7LA�9XA�=qA�9XA�9XA�A�A�E�A�C�A�E�A�K�A�M�A�M�A�S�A�VA�ZA�^5A�bNA�bNA�hsA�n�A�n�A�hsA�VA�E�A�7LA�&�A��A�{A�JA�
=A�
=A�%A�  A���A���A��A��A��yA��yA��TA��HA��TA��mA��TA��;A��TA��HA��/A��#A��/A��A��#A��#A��A���A���A���A���A���A�ȴA�A���A�ĜA�ĜA���A�A�ĜA���AξwA�ĜA���AμjA���A���AμjAξwA���AξwAμjA���AμjAμjAξwAκ^AθRAμjAκ^AζFAκ^AμjAθRAζFAκ^AθRAδ9AθRAκ^Aδ9Aδ9AθRAζFAβ-AζFAθRAδ9Aδ9AζFAζFAΰ!Aβ-Aδ9Aΰ!AήAβ-Aΰ!AάAήAΰ!AΥ�AΝ�AΛ�AΙ�AΓuAΓuAΗ�AΕ�AΓuAΓuAΕ�AΏ\AΏ\AΕ�AΑhAΏ\AΓuAΓuAΏ\AΏ\AΏ\AΑhA·+A΁A΃A΁A�z�A�p�A�n�A�ffA�bNA�ffA�dZA�^5A�`BA�ZA�O�A�O�A�O�A�E�A�C�A�E�A�=qA�5?A�/A�$�A��A�oA�
=A�  A��A��#A;wAͮA͛�A͋DA�r�A�I�A�9XA���A̕�A�x�A�O�A�"�A�
=A�A��A˶FA�bNA�bA��mA���A���Aʉ7A�-A��`AɬAɃA�bNA�(�A���A���Aș�A�x�A�jA�O�A�?}A�/A��A�bA�1A���A��A��A��A��mA��;A��
A���AǶFAǙ�AǃA�\)A���Aư!AƟ�Aƕ�AƅA�v�A�n�A�dZA�M�A�/A�{A��A��;A�ȴAŶFAũ�Aŕ�A�~�A�hsA�VA�C�A�1'A�$�A�{A�VA��A���Aġ�Aę�AāA�z�A�dZA�I�A�$�A��A�bA���A��A��TA��/A���AìA�|�A�S�A�9XA�+A�VA��mA���Aº^A¶FA¬A�A�x�A�;dA�  A���A��hA���A�hsA�5?A�&�A�$�A��A�VA���A��TA��A���A�A��wA���A��jA��FA���A���A���A���A��uA��A�|�A�z�A�z�A�hsA�Q�A�O�A�A�A�$�A��A��A�bA�VA�
=A�
=A�A���A��A��`A���A�A��jA��jA��RA��9A��-A��!A���A�v�A�  A���A�hsA�  A��!A���A��uA�z�A�bNA�ZA�XA�Q�A�O�A�O�A�7LA��;A��A�dZA���A���A�|�A�`BA�9XA���A���A���A�x�A�=qA�{A���A�t�A�$�A��A��A��`A���A��FA���A��A�ZA�C�A��A��A�ƨA�l�A�I�A�=qA�?}A�+A�bA���A��`A���A��-A�~�A�^5A�7LA��A���A��
A��^A���A���A��uA��A�dZA�A�A�7LA�/A�$�A�oA���A��mA���A���A���A��A�I�A�1'A��A�1A���A��;A��;A��/A��;A��;A���A��wA��-A��A��A���A��DA�VA�1'A��A�JA�1A��A��+A���A�r�A��A��uA�^5A�E�A�/A�"�A�1A��;A��^A��A���A�v�A�(�A��`A�ĜA���A��+A�jA�K�A�+A��A�1A���A��mA�ȴA���A��DA�Q�A�(�A�oA�
=A�A���A��`A��FA�t�A���A�A���A��\A�r�A�\)A�O�A�?}A�+A�bA��A��A��A�ffA�O�A�1'A�JA���A���A��PA�`BA�9XA�/A��A�  A���A���A���A���A��A���A���A��A��mA��^A���A��+A�p�A�I�A��A��HA���A�O�A�(�A��A�bA��;A���A�r�A�dZA�;dA�&�A�"�A��A�%A��!A��mA�XA�"�A��;A��^A��A���A���A��7A�t�A�ZA�=qA�JA��A��;A���A���A���A�v�A�+A�%A��A��^A���A�t�A�G�A�-A� �A�oA��`A��DA�/A��A���A�n�A�Q�A�I�A�I�A�?}A�
=A���A�?}A��
A���A�~�A�-A��HA���A�x�A�bNA�I�A�;dA��A�A��A��;A���A��wA��-A��uA�hsA�/A��yA��FA���A��DA�r�A�dZA�XA�C�A�"�A�A�^5A�;dA��A��A��A�XA�G�A�-A�bA���A��jA��A��PA�z�A�XA���A��jA���A�hsA�ZA�G�A�VA���A��PA�jA�A�A��A��
A��A�M�A�=qA��A�{A�JA���A��#A��FA���A��7A�ZA�C�A�1A�%A��HA�z�A�~�A�/A�l�A�ĜA�/A��A�A��/A���A�A��A�p�A�=qA��A���A�n�A�G�A�E�A�E�A�?}A�+A�%A���A�p�A�33A�A���A��A��;A��A���A�A��RA��9A���A���A�r�A�jA�jA�M�A�E�A�"�A��hA�7LA��TA��FA��A�S�A���A���A�XA��A��/A��DA�r�A�XA��A��`A��wA���A�ffA�"�A��A��A�ZA�1'A�(�A�"�A��A��A��A���A�XA��A�JA�A�hAoA~�A}&�A|VA{�A{�^A{��A{\)A{VAz�Az��Azn�AzI�Az-Az{Ay��Ay�TAy��Ay��Ay��Ayt�Ayl�AyXAy%Ax��Ax^5AxJAwp�Av��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�B
YB
�B
$B
YB
�B
$B
$B
�B
�B
$B
YB
YB
 �B
0!B
CaB
OBB
x�B
�VB
��B
�-B
�aB
�9B
�<B
�zB
�)B
уB
רB
��B
��B�B�zBxB�BBxBBB
�BBDBB
�B
�B
=B	�B	lB
�B�B$�B&LB)�B-�B1�B9�B?BF�BCaBB�B@�BD�BC-B=�B7B;�BD�BHKBE�BA�BE�BG�B7B>wBA BB�BA B>wB9$B6FB(XB�B�B��B��B�?B�<B��B��B�FB��Bl"BYBH�B~B
��B
��B
ߤB
�B
��B
��B
�GB
v`B
^�B
OBB
HKB
=�B
+�B
 \B
.B
fB	�B	�B	�`B	�gB	�qB	�UB	��B	�xB	��B	�B	s�B	_�B	S�B	<�B	/�B	&�B	%�B	"�B	!bB	7B	�B	�B	�B	�B		�B	�B	�B	SB	�B	�B	�B	uB	�B	�B	�B	fB	
=B	�B	�B	~B	 \B	+�B	7B	1[B	3�B	49B	5tB	8�B	;dB	<B	C�B	QNB	h�B	v�B	zDB	|�B	� B	��B	zDB	t�B	k�B	]�B	S�B	PB	S&B	`�B	\)B	F?B	OBB	PB	OB	I�B	GzB	I�B	N�B	O�B	P�B	T�B	\�B	g�B	pB	{B	��B	�B	� B	�fB	�%B	�YB	� B	��B	��B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	�6B	�6B	��B	��B	�!B	�-B	��B	�nB	��B	�B	��B	��B	�wB	�B	�CB	�B	�wB	�6B	��B	�OB	�B	��B	��B	�FB	�FB	�tB	�RB	�B	��B	��B	��B	�zB	��B	��B	�<B	�HB	��B	��B	��B	��B	�}B	��B	�'B	��B	�wB	�qB	�B	�}B	�B	�[B	B	�[B	�B	�XB	ʌB	ʌB	��B	ΥB	ӏB	�?B	�&B	�2B	��B	�aB	�,B	��B	�
B	�gB	��B	�2B	��B	ԕB	�aB	�B	�B	�KB	�)B	�dB	�5B	�vB	�B	��B	�B	��B	�,B	�2B	�
B	�B	�KB	�B	�B	��B	�QB	�B	�B	��B	�B	�B	�B	�WB	��B	�)B	�"B	��B	�5B	�B	�B	��B	�B	�GB	��B	��B	�B	�TB	�B	��B	��B	��B	��B	�lB	��B	��B	�B	��B	�"B	��B
 4B
  B
�B
�B
uB
�B
�B
�B
�B
B
�B
�B
+B
_B
�B
�B
�B
	lB
B
�B
B
�B
B
B
PB
PB
�B
"B
(B
�B
�B
 B
�B
bB
�B
hB
�B
�B
�B
�B
�B
MB
�B
�B
B
�B
SB
SB
�B
kB
�B
=B
�B
�B
qB
=B
�B
CB
�B
CB
�B
B
�B
OB
�B
�B
�B
VB
 'B
�B
 'B
!�B
!�B
!�B
!�B
"4B
"4B
"�B
"�B
#B
#B
#�B
$tB
$tB
%B
&�B
'B
'B
'B
'�B
'RB
'�B
(XB
)*B
)_B
)�B
*eB
)�B
*0B
*eB
*0B
+kB
+6B
+B
*�B
,=B
,qB
+�B
+�B
,=B
,qB
,qB
,�B
.B
-CB
.B
.}B
.IB
/OB
/�B
0�B
0�B
0�B
1'B
1'B
1'B
1[B
2-B
2�B
2-B
2�B
2aB
33B
2�B
3hB
5?B
5tB
5tB
5�B
5�B
5�B
5�B
6FB
7LB
7�B
7�B
8B
8RB
8�B
8�B
8�B
:�B
;dB
;�B
<B
=B
=qB
=qB
=<B
>B
>�B
>�B
>BB
>�B
>�B
?B
?B
?HB
?}B
?}B
?}B
?�B
?�B
@�B
@�B
B�B
B�B
B[B
B�B
C-B
C-B
CaB
C-B
C-B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D3B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
K�B
L�B
MB
MjB
NB
N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>|�>|PH>��[?�~(B
$B
�B
SB
+B
�B
�B
FB
1B
�B
�B
�B
B
+B
�B
�B
$B
+B
�B
$B
�B
�B
�B
�B
�B
YB
�B
�B
�B
B
�B
�B
B
B
�B
�B
YB
�B
+B
�B
�B
SB
�B
�B
�B
YB
�B
YB
B
"4B
#B
&B
(XB
'�B
%�B
.�B
2�B
2�B
;�B
?HB
>BB
C-B
A�B
E�B
GEB
GB
GzB
K�B
L0B
OBB
P�B
W?B
[�B
f2B
qvB
��B
��B
�+B
�rB
�JB
��B
��B
��B
��B
��B
��B
�{B
�YB
��B
��B
�qB
�=B
�qB
��B
�nB
��B
�kB
��B
�}B
��B
��B
��B
�-B
��B
�UB
�aB
�FB
�tB
��B
�XB
��B
��B
�BB
B
��B
ƨB
�zB
��B
�RB
�RB
ǮB
��B
�#B
�KB
�0B
͟B
�6B
�B
уB
бB
�&B
��B
�?B
�?B
خB
�B
�mB
�B
�EB
��B
�EB
��B
��B
�dB
ߤB
�
B
�B
��B
�fB
��B
�PB�B.B_B"�B:*BA BYB��B�B�QB��BAB
�B
�B�B�BDB�BPBPB�BB�B�BxB�BPB�B
rBB~B
�B
�B�BB
	B�B
�B
=BB�B
	B
�BBxB	�BxBxB
=B
rB�B
�B
=BDB�B	�B
�BJB
�B
�BBDB
	BxB�B
	BxB�B
	BxB�B
rB
�BB
�B
	BBB
	B
�BB
rB	�BxB�B
	BBB
rB
	BxBxB
	B
	B�BB	�BxB�B
rB	�B�B
rB	�BB
�B
=B
	B�B
�B	7B
=BDB
�B�B
�BxB	7B
=BB	B	�B
�B
�B
	BfB	lBB	�B1B
rBJB	lB
	B
rB1B	�B
rB�B	�BDB	7BfB
rB	�B�B	7B
rB	�B�B	lB	�B	�B�B	BBJB�B�B	�B�B�B�B�BBuBB�BMB�B�B7BB 'B1BB�B#nB$�B#nB%�B#�B%�B($B%zB*eB*�B$�B%�B'�B'B&B'�B&LB%FB&LB&�B&�B$tB%�B&�B&LB%FB'B'�B'�B,=B49B0!B-wB+6B+6B*�B+6B+6B.B+�B-�B0�B/OB-�B.�B,qB/�B2�B1'B.�B2aB0!B/B/�B0�B6zB5�B3�B2aB4�B4�B4�B7�B7�B8�B:*B:*B8�B8�B8�B8�B:^B<B<jB:^B:�B<6B?HB@�B>B;�B<6B<6BC-B@�BDgB@�BH�BJ�BXEBE9BD3BB[BB�BC�BEmBHKBB�BA�BEBD3BB�BB'BA�BCaBE�BB[BA�BB�BF?BC-BC�BA�BC-BG�BA�BE�BCaBC�BA�BB[BB�BCaB@�B@�BA�BDgBB[BB�BB'BA�B?�B?HB?�B?�B>�B<�BD�BRTBC�BHBM�BGEB@�B@�BA�BC-B?B=B>B=B:^B=�BF?B\�BJ�BF�BB�B@�B9�B=�B?}B=�B;0B>wB;�B=�B>BB@�B>�B5�B7�B2�B6B6zB9�B4B8�B3hB8B:�B7�BC-B;�B<jB9�B?}B=�B>�B=<B?B?HBIBEBH�BH�BI�BHBI�BH�BG�BGzBH�BM�BHKBF�BEBE9BH�BF�BA�BD�BA BA�BGzBHKBC�BA�BF�BC�BA�B?B@�B?�B?}BC-BE9BE�BB�BC-BF�BE�BOBBF�BGBDgBC-BC�BU�BT�BD�BN<B8RB<jB8B;�B6B7�B8�B7�B3hB1�B=B=�B;�B:�B>BB?HB?B>�B?B=�B@�B>BB@B@BB'BA�BH�B?HB?�B@�B@B?BA�BB'BHKBK^BB�B?�B@�BB'BAUB?�B?HB?}B?}BB�BCaB>�B=�B=B?�B<6B;�BB�BB�B>BB<jB:�B<jB=B7�B7B6�B8RB7�B6B5?B6B9�B:^B5?B2�B5B7LB2�B4B5?B4�B+B'B$@B'B)*B�B!�B$B"hB	BB=B)�B0�B$tB:BFB�BDB	lB
rB	�B
�B
�BB	B%BMBBB �B�BSB��B�]B��B�B��B�B��B�5B�]B�GB�B�B�WB�B�`B�B�B�mB�
B�pB�B�HB�}B�B�BΥBŢB��B�B�qB��B�LB��B�B�B�?B�9B�'B��B�-B��B�FB��B��B�_B�*B�B�RB�$B��B�LB��B��B�-B�VB�nB��B��B��B��B�B��B��B��B�=B��B��B�B��B�B}VB~�B|B�YB��Bo5Bx�BlWBn�Bl"Bu%Bd�Bb�Bb�B^5B_;B_pB[�BVBXEBT�B\)BP�B[#BM�BK�BZ�B>�BO�BX�BQ�B:�B%FB'�B,�B(�B)�B%�B�B
�.B�B�B�B
�	B
�`B
�ZB
�B
�B
��B
�B
�B
�B
�KB
�>B
��B
�B
�&B
�B
�B
�B
�NB
��B
��B
��B
��B
�/B
�BB
خB
֡B
��B
̘B
רB
��B
�0B
�OB
�B
��B
�BB
�tB
��B
��B
��B
��B
��B
��B
�YB
�SB
��B
�{B
�VB
��B
��B
�SB
�B
��B
�;B
��B
y�B
}"B
�;B
s�B
r|B
rGB
rB
s�B
z�B
sB
oiB
[#B
[�B
Y�B
[WB
ZB
V�B
S�B
S�B
Q�B
OB
N�B
K�B
L0B
IB
J�B
H�B
GB
E�B
GB
IRB
GB
C-B
D�B
H�B
I�94444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�94444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022040609095920220406090959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022041604010920220416040109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022041604010920220416040109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                